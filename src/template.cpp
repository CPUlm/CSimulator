#include<stdio.h>
#include<cstdint>
#include<vector>
#include<inttypes.h>

const long long nbSteps = $NB_STEPS$;

class Gate	{
		bool isCalculated;
		uint32_t value;
		uint32_t (*fct) ();

	public:
		bool mustBeCalculated;

		uint32_t calcGate()	{
			if (!isCalculated)	{
				value = fct();
				isCalculated = true;
			}
			return value;
		}

		void init()	{
			isCalculated = false;
		}
		Gate(bool isOutput_) {
			mustBeCalculated = isOutput_;
			value = 0;
		}
		void setFct( uint32_t (*f) () )	{
			fct = f;
		}
		void readInput(int size)	{
			value = 0;
			for (int i = 0; i < size; i++)	{
				char cara;
				scanf(" %c", &cara);
				if (cara == '1')
					value |= (1 << i);
			}
			isCalculated = true;
		}
		void show(int size)	{
			uint32_t copyVal = calcGate();
			for (int i = 0; i < size; i++)	{
				if (copyVal & (1<<i))	printf("1");
				else	printf("0");
			}
		}

};
class GateReg	{
		bool isCalculated;
		uint32_t value [2];
		uint32_t (*fct) ();

	public:
		bool mustBeCalculated;

		uint32_t calcGate()	{
			if (!isCalculated)	{
				value[0] = fct();
				isCalculated = true;
			}
			return value[0];
		}
		uint32_t getOldValue()	{
			return value[1];
		}

		void init()	{
			isCalculated = false;
			value[1] = value[0];
		}
		GateReg(bool isOutput_) {
			mustBeCalculated = true;
			value[0] = 0;
			value[1] = 0;
		}
		void setFct( uint32_t (*f) () )	{
			fct = f;
		}
		void readInput(int size)	{
			value[0] = 0;
			for (int i = 0; i < size; i++)	{
				char cara;
				scanf(" %c", &cara);
				if (cara == '1')
					value[0] |= (1 << i);
			}
			isCalculated = true;
		}
		void show(int size)	{
			uint32_t copyVal = calcGate();
			for (int i = 0; i < size ; i++)	{
				if (copyVal & (1<<i))	printf("1");
				else	printf("0");
			}
		}



};
class Memory	{
	private:
		std::vector<uint32_t>  mem;
	public:
		uint32_t get(uint32_t addr)	{
			if (addr >= (uint32_t)mem.size())
				mem.resize(addr+1, 0);
			return mem[addr];
		}
		void set(uint32_t addr, uint32_t val)	{
			if (addr >= (uint32_t)mem.size())
				mem.resize(addr+1, 0);
			mem[addr] = val;
		}
		void initRom(int addrSize, int wordSize)	{
			for (unsigned long long addr = 0; addr < (1ull << addrSize); addr++)	{
				uint32_t value = 0;
				for (int i = 0; i < wordSize ; i++)	{
					char cara;
					scanf(" %c", &cara);
					if (cara == '1')
						value |= (1 << i);
				}
				set(addr,value);
			}
		}

};

$MEM_DEF$

$GATE_DEF$

$FCT_DEF$

int main()	{
$FCT_SET$

$READ_ROM$

	for (long long iStep = 0; iStep != nbSteps; iStep++)	{
		printf("Step %lld:\n", iStep+1);

$INIT$

$INPUT$

$CALC$

$WRITE_RAM$

$OUTPUT$
	}
}

