#include <Eigen/Core>
#include <Eigen/Dense>
#include <Eigen/Sparse>
#include <Eigen/SparseQR>
#include <algorithm>
#include <iostream>
#include <random>
#include <set>

void sampleEdges(int npoints, std::set<std::pair<int, int>> &edges)
{
    edges.clear();
    std::random_device r;
    std::default_random_engine e1(r());
    std::uniform_int_distribution<int> uniform_dist(0, npoints - 1);
    int nedges = 3 * npoints;
    for (int i = 0; i < nedges; i++) {
        bool ok = false;
        while (!ok) {
            int v1 = uniform_dist(e1);
            int v2 = uniform_dist(e1);
            if (v1 > v2)
                std::swap(v1, v2);
            std::pair<int, int> p(v1, v2);
            if (v1 != v2 && !edges.count(p)) {
                edges.insert(p);
                ok = true;
            }
        }
    }
}

void sampleRestLengths(int nedges, Eigen::VectorXd &lens)
{
    lens.resize(nedges);
    std::random_device r;
    std::default_random_engine e1(r());
    std::uniform_real_distribution<double> dist(0, 1.0);
    for (int i = 0; i < nedges; i++)
        lens[i] = dist(e1);
}

struct Config {
    Eigen::MatrixXd pos;
    Eigen::MatrixXi edges;
    Eigen::VectorXd restlens;
};

double energy(const Config &c, Eigen::VectorXd &deriv,
              std::vector<Eigen::Triplet<double>> &hessian,
              std::vector<Eigen::Triplet<double>> &mixedderiv)
{
    int npoints = c.pos.rows();
    deriv.resize(3 * npoints);
    deriv.setZero();
    mixedderiv.clear();
    hessian.clear();
    double result = 0;

    int nedges = c.edges.rows();
    for (int i = 0; i < nedges; i++) {
        int idx1 = c.edges(i, 1);
        int idx0 = c.edges(i, 0);
        Eigen::Vector3d delta = c.pos.row(idx1) - c.pos.row(idx0);
        double e = 0.5 * (delta.norm() - c.restlens[i]) *
                   (delta.norm() - c.restlens[i]);
        result += e;
        Eigen::Vector3d local =
                (delta.norm() - c.restlens[i]) * delta / delta.norm();
        deriv.segment<3>(3 * idx1) += local;
        deriv.segment<3>(3 * idx0) -= local;
        for (int j = 0; j < 3; j++) {
            mixedderiv.push_back(
                    Eigen::Triplet<double>(3 * idx1 + j, i, local[j]));
            mixedderiv.push_back(
                    Eigen::Triplet<double>(3 * idx0 + j, i, -local[j]));
        }

        Eigen::Matrix3d I;
        I.setIdentity();
        Eigen::Matrix3d localHess =
                delta * delta.transpose() / delta.squaredNorm() +
                (delta.norm() - c.restlens[i]) *
                        (I - delta * delta.transpose() / delta.squaredNorm()) /
                        delta.norm();
        for (int j = 0; j < 3; j++) {
            for (int k = 0; k < 3; k++) {
                hessian.push_back(Eigen::Triplet<double>(
                        3 * idx0 + j, 3 * idx0 + k, localHess(j, k)));
                hessian.push_back(Eigen::Triplet<double>(
                        3 * idx0 + j, 3 * idx1 + k, -localHess(j, k)));
                hessian.push_back(Eigen::Triplet<double>(
                        3 * idx1 + j, 3 * idx0 + k, -localHess(j, k)));
                hessian.push_back(Eigen::Triplet<double>(
                        3 * idx1 + j, 3 * idx1 + k, localHess(j, k)));
            }
        }
    }
    return result;
}

void relaxConfiguration(Config &c)
{
    Eigen::VectorXd deriv;
    std::vector<Eigen::Triplet<double>> hess;
    std::vector<Eigen::Triplet<double>> dummy;
    double e = energy(c, deriv, hess, dummy);
    while (deriv.norm() > 1e-8) {
        // std::cout << "energy " << e << " force " << deriv.norm() <<
        // std::endl;
        int npoints = c.pos.rows();
        Eigen::SparseMatrix<double> H(3 * npoints, 3 * npoints);
        H.setFromTriplets(hess.begin(), hess.end());
        Eigen::SparseQR<Eigen::SparseMatrix<double>, Eigen::COLAMDOrdering<int>>
                solver(H);
        Eigen::VectorXd delta = solver.solve(deriv);
        for (int i = 0; i < npoints; i++) {
            for (int j = 0; j < 3; j++) {
                c.pos(i, j) -= delta[3 * i + j];
            }
        }
        e = energy(c, deriv, hess, dummy);
    }
}

void alignConfig(const Config &dst, Config &src)
{
    Eigen::Vector3d centroidsrc(0, 0, 0);
    Eigen::Vector3d centroiddst(0, 0, 0);
    int npts = src.pos.rows();
    for (int i = 0; i < npts; i++) {
        centroidsrc += src.pos.row(i);
        centroiddst += dst.pos.row(i);
    }
    centroidsrc /= npts;
    centroiddst /= npts;
    Eigen::MatrixXd A(3, npts);
    Eigen::MatrixXd B(3, npts);
    for (int i = 0; i < npts; i++) {
        A.col(i) = src.pos.row(i).transpose() - centroidsrc;
        B.col(i) = dst.pos.row(i).transpose() - centroiddst;
    }
    Eigen::Matrix3d M = B * A.transpose();
    Eigen::JacobiSVD<Eigen::MatrixXd> svd(
            M, Eigen::ComputeFullU | Eigen::ComputeFullV);
    Eigen::Matrix3d sigma;
    sigma.setIdentity();
    double sign = svd.matrixU().determinant() * svd.matrixV().determinant();
    // sigma(2, 2) = sign;
    Eigen::Matrix3d R = svd.matrixU() * sigma * svd.matrixV().transpose();
    for (int i = 0; i < npts; i++) {
        Eigen::Vector3d newv =
                centroiddst + R * (src.pos.row(i).transpose() - centroidsrc);
        src.pos.row(i) = newv.transpose();
    }
}

int main()
{
    int npoints = 10;
    Eigen::MatrixXd pos(npoints, 3);
    pos.setRandom();

    std::set<std::pair<int, int>> edges;
    sampleEdges(npoints, edges);

    Eigen::MatrixXd equipos;
    Config c;
    c.pos = pos;
    c.edges.resize(edges.size(), 2);
    int idx = 0;
    for (std::pair<int, int> p : edges) {
        c.edges(idx, 0) = p.first;
        c.edges(idx, 1) = p.second;
        idx++;
    }
    int nedges = edges.size();
    sampleRestLengths(nedges, c.restlens);
    relaxConfiguration(c);

    Eigen::VectorXd cutdistance(nedges);
    for (int i = 0; i < nedges; i++) {
        Config c2 = c;
        c2.edges.resize(nedges - 1, 2);
        c2.restlens.resize(nedges - 1);
        int idx = 0;
        for (int j = 0; j < nedges; j++) {
            if (j == i)
                continue;
            c2.edges.row(idx) = c.edges.row(j);
            c2.restlens[idx] = c.restlens[j];
            idx++;
        }
        relaxConfiguration(c2);

        alignConfig(c, c2);

        double dist = (c2.pos - c.pos).norm();
        cutdistance[i] = dist;
    }

    Eigen::VectorXd deriv;
    std::vector<Eigen::Triplet<double>> hess;
    std::vector<Eigen::Triplet<double>> mixed;
    double e = energy(c, deriv, hess, mixed);

    Eigen::SparseMatrix<double> H(3 * npoints, 3 * npoints);
    Eigen::SparseMatrix<double> MD(3 * npoints, nedges);

    H.setFromTriplets(hess.begin(), hess.end());
    MD.setFromTriplets(mixed.begin(), mixed.end());
    Eigen::SparseQR<Eigen::SparseMatrix<double>, Eigen::COLAMDOrdering<int>>
            solver(H);

    Eigen::VectorXd predicteddist(nedges);

    for (int i = 0; i < nedges; i++) {
        Eigen::VectorXd column = MD.col(i);
        Eigen::VectorXd deltaq = solver.solve(column);
        double dqnorm = deltaq.norm();
        predicteddist[i] = dqnorm;
    }

    Eigen::VectorXd strains(nedges);
    for (int i = 0; i < nedges; i++) {
        double len =
                (c.pos.row(c.edges(i, 1)) - c.pos.row(c.edges(i, 0))).norm();
        double strain = (len - c.restlens[i]) / c.restlens[i];
        strains[i] = strain;
    }

    for (int i = 0; i < nedges; i++) {
        std::cout << i << " " << cutdistance[i] << " " << predicteddist[i]
                  << " " << strains[i] << std::endl;
    }
}