local main = {}

function main.fun_0() end

function main.fun_1()
	main.fun_0()
	io.write("C")
end

main.fun_1()
