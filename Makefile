init:
	git submodule update --init
	cd rocket-chip && git submodule update --init hardfloat cde

compile:
	mill -i HuanCun.compile

test:
	mill -i HuanCun.test.test


test-top-l2:
	mill -i HuanCun.test.runMain huancun.TestTop_L2 -td build

test-top-l2standalone:
	mill -i HuanCun.test.runMain huancun.TestTop_L2_Standalone -td build

test-top-l2l3:
	mill -i HuanCun.test.runMain huancun.TestTop_L2L3 -td build

test-top-fullsys:
	mill -i HuanCun.test.runMain huancun.TestTop_FullSys -td build

basic-test:
	mill -i HuanCun.test.testOnly -o -s huancun.ConnectionTester

bsp:
	mill -i mill.bsp.BSP/install

idea:
	mill -i mill.scalalib.GenIdea/idea

clean:
	rm -rf ./build

reformat:
	mill -i __.reformat

checkformat:
	mill -i __.checkFormat
