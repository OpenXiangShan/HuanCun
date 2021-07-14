init:
	git submodule update --init
	cd rocket-chip && git submodule update --init hardfloat api-config-chipsalliance

compile:
	mill -i HuanCun.compile

test:
	mill -i HuanCun.test.test

basic-test:
	mill -i HuanCun.test.testOnly -o -s huancun.ConnectionTester

bsp:
	mill -i mill.bsp.BSP/install

clean:
	git clean -fd

reformat:
	mill -i __.reformat

checkformat:
	mill -i __.checkFormat
