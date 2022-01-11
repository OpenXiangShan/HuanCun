init:
	git submodule update --init
	cd rocket-chip && git submodule update --init hardfloat api-config-chipsalliance

compile:
	mill -i HuanCun.compile

test:
	mill -i HuanCun.test.test


test-top-l2:
	mill -i HuanCun.test.runMain huancun.TestTop_L2 -td build
	mv build/TestTop_L2.v build/TestTop.v

test-top-l2l3:
	mill -i HuanCun.test.runMain huancun.TestTop_L2L3 -td build
	mv build/TestTop_L2L3.v build/TestTop.v

basic-test:
	mill -i HuanCun.test.testOnly -o -s huancun.ConnectionTester

bsp:
	mill -i mill.bsp.BSP/install

clean:
	rm -rf ./build

reformat:
	mill -i __.reformat

checkformat:
	mill -i __.checkFormat
