#include<stdio.h>
#include<cstdint>
#include<vector>

const long long nbSteps = 4;

class Gate	{
		bool isCalculated;
		uint32_t value;
		uint32_t (*fct) ();

	public:
		bool mustBeCalculated;
		bool isOutput;

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
		Gate(bool isOutput_ ) {
			mustBeCalculated = isOutput_;
			isOutput = isOutput_;
			value = 0;
		}
		void setFct( uint32_t (*f) () )	{
			fct = f;
		}
		void readInput()	{
			scanf("%d", &value);
		}
};
class GateReg	{
		bool isCalculated;
		uint32_t value [2];
		uint32_t (*fct) ();

	public:
		bool mustBeCalculated;
		bool isOutput;

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
			isOutput = isOutput_;
			value[0] = 0;
			value[1] = 0;
		}
		void setFct( uint32_t (*f) () )	{
			fct = f;
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
				for (int i = 0; i < wordSize; i++)	{
					char cara;
					scanf(" %c", &cara);
					if (cara == '1')
						value |= (1 << i);
				}
				set(addr,value);
			}
		}

};

Memory mem1;


Gate a (true);
GateReg b (true);
Gate c (true);
Gate d (true);

uint32_t fct_a () {return b.getOldValue();}
uint32_t fct_b () {return ~(a.calcGate());}
uint32_t fct_c () {return  (b.calcGate());}
uint32_t fct_d () {return mem1.get(10);}


int main()	{
	a.setFct(&fct_a);
	b.setFct(&fct_b);
	c.setFct(&fct_c);
	d.setFct(&fct_d);


	for (long long iStep = 0; iStep != nbSteps; iStep++)	{
		printf("step %lld:\n", iStep+1);

		a.init();
		b.init();
		c.init();
		d.init();

		if (a.mustBeCalculated) a.calcGate();
		if (b.mustBeCalculated) b.calcGate();
		if (c.mustBeCalculated) c.calcGate();
		if (d.mustBeCalculated) d.calcGate();

		if (true) mem1.set(10, 42);

		if (a.isOutput) printf("=> a = %08X\n", a.calcGate());
		if (b.isOutput) printf("=> b = %08X\n", b.calcGate());
		if (c.isOutput) printf("=> c = %08X\n", c.calcGate());
		if (d.isOutput) printf("=> d = %08X\n", d.calcGate());


	}

}
