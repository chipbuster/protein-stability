using Test
include("idealchain.jl")


function test_flip_point()
    # Test 1: flip across y = -x
    e1 = [-1,1]
    e2 = [1,-1]

    for i in 1:1000
        testpt = rand(Float64,2)
        res = flip_point_about_line(testpt, (e1,e2))
        (x,y) = res
        res = [-y,-x]
        if norm(testpt - [-y,-x]) > 1e-6
            println(res,testpt)
            return false
        end
    end

    # Test 2: flip randomly, asserting that the distance between the point
    # and its reflection lies along the line.
    e1 = [0;0]
    e2 = [0;0]
    z = e2 - e1
    while(norm(e2 - e1) < 0.5)
        e1 = randn(Float64,2)
        e2 = randn(Float64,2)
        z = e2 - e1
    end


    for i in 1:1000
        testpt = 10 * randn(Float64,2)
        res = flip_point_about_line(testpt, (e1,e2))
        midpt = (testpt + res) ./ 2

        # Compute the form of the reflection line
        m = z[2] / z[1]
        b = e1[2] - e1[1] * z[2] / z[1]

        if m > 30000  # This will probably hit floating point issues
            continue
        end

        e2d = e2[2] - (m * e2[1] + b)
        e1d = e1[2] - (m * e1[1] + b)

        @assert(abs(e2d) < 1e-5)
        @assert(abs(e1d) < 1e-5)

        dym = midpt[2] - (m * midpt[1] + b)

        if dym > 1e-6
            xs = collect(-20:1:20)
            ys = m .* xs .+ b
            plot(xs,ys)
            scatter!([e1[1],e2[1]],[e1[2],e2[2]])
            scatter!([x[1] for x in [testpt, res, midpt]], [x[2] for x in [testpt, res, midpt]])
            println("Error in y-coord was $(dym)")
            println("y = $(m) x + $(b)")
            println("e1 = $(e1)")
            println("e2 = $(e2)")
            println("pt = $(testpt)")
            gui()
            return false
        end
    end

    return true
end


@test test_flip_point()
