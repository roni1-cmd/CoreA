/**
 * CoreA CPU Feature Detection
 * Header file for CPU feature detection functionality
 */

#ifndef _KERNEL_CPU_FEATURES_H
#define _KERNEL_CPU_FEATURES_H

#include <kernel/kernel.h>

/**
 * Initialize CPU feature detection
 * Should be called early in kernel initialization
 */
void detect_cpu_features(void);

/**
 * Check if CPU supports a specific feature
 * @param feature_id Feature identifier (use CPUFEATURE_* constants)
 * @return 1 if supported, 0 if not
 */
int cpu_has_feature(int feature_id);

/**
 * Get CPU vendor string
 * @return Pointer to null-terminated vendor string
 */
const char* get_cpu_vendor(void);

/**
 * Get CPU brand string
 * @return Pointer to null-terminated brand string
 */
const char* get_cpu_brand(void);

/**
 * Get number of CPU cores
 * @return Number of CPU cores
 */
int get_cpu_cores(void);

/* CPU Feature Constants */

/* Standard Features (ECX) */
#define CPUFEATURE_SSE3        0x000    /* SSE3 Instructions */
#define CPUFEATURE_PCLMULQDQ   0x001    /* PCLMULQDQ Instruction */
#define CPUFEATURE_DTES64      0x002    /* 64-bit Debug Store */
#define CPUFEATURE_MONITOR     0x003    /* MONITOR/MWAIT instructions */
#define CPUFEATURE_DS_CPL      0x004    /* CPL Qualified Debug Store */
#define CPUFEATURE_VMX         0x005    /* Virtual Machine Extensions */
#define CPUFEATURE_SMX         0x006    /* Safer Mode Extensions */
#define CPUFEATURE_EST         0x007    /* Enhanced SpeedStep */
#define CPUFEATURE_TM2         0x008    /* Thermal Monitor 2 */
#define CPUFEATURE_SSSE3       0x009    /* SSSE3 Instructions */
#define CPUFEATURE_CNXT_ID     0x00A    /* L1 Context ID */
#define CPUFEATURE_FMA         0x00C    /* Fused Multiply Add */
#define CPUFEATURE_CMPXCHG16B  0x00D    /* CMPXCHG16B Instruction */
#define CPUFEATURE_XTPR        0x00E    /* Send Task Priority Messages */
#define CPUFEATURE_PDCM        0x00F    /* Perf/Debug Capability MSR */
#define CPUFEATURE_PCID        0x011    /* Process-context Identifiers */
#define CPUFEATURE_DCA         0x012    /* Direct Cache Access */
#define CPUFEATURE_SSE41       0x013    /* SSE4.1 Instructions */
#define CPUFEATURE_SSE42       0x014    /* SSE4.2 Instructions */
#define CPUFEATURE_X2APIC      0x015    /* x2APIC Support */
#define CPUFEATURE_MOVBE       0x016    /* MOVBE Instruction */
#define CPUFEATURE_POPCNT      0x017    /* POPCNT Instruction */
#define CPUFEATURE_TSC_DEADLINE 0x018   /* TSC Deadline */
#define CPUFEATURE_AES         0x019    /* AES Instructions */
#define CPUFEATURE_XSAVE       0x01A    /* XSAVE/XRSTOR */
#define CPUFEATURE_OSXSAVE     0x01B    /* XSAVE Enabled by OS */
#define CPUFEATURE_AVX         0x01C    /* Advanced Vector Extensions */
#define CPUFEATURE_F16C        0x01D    /* 16-bit FP Conversion */
#define CPUFEATURE_RDRAND      0x01E    /* RDRAND Instruction */

/* Standard Features (EDX) */
#define CPUFEATURE_FPU         0x100    /* Floating Point Unit */
#define CPUFEATURE_VME         0x101    /* Virtual 8086 Mode Enhancements */
#define CPUFEATURE_DE          0x102    /* Debugging Extensions */
#define CPUFEATURE_PSE         0x103    /* Page Size Extension */
#define CPUFEATURE_TSC         0x104    /* Time Stamp Counter */
#define CPUFEATURE_MSR         0x105    /* Model Specific Registers */
#define CPUFEATURE_PAE         0x106    /* Physical Address Extension */
#define CPUFEATURE_MCE         0x107    /* Machine Check Exception */
#define CPUFEATURE_CX8         0x108    /* CMPXCHG8B Instruction */
#define CPUFEATURE_APIC        0x109    /* APIC On-Chip */
#define CPUFEATURE_SEP         0x10B    /* SYSENTER/SYSEXIT */
#define CPUFEATURE_MTRR        0x10C    /* Memory Type Range Registers */
#define CPUFEATURE_PGE         0x10D    /* Page Global Enable */
#define CPUFEATURE_MCA         0x10E    /* Machine Check Architecture */
#define CPUFEATURE_CMOV        0x10F    /* Conditional Move Instructions */
#define CPUFEATURE_PAT         0x110    /* Page Attribute Table */
#define CPUFEATURE_PSE36       0x111    /* 36-bit Page Size Extension */
#define CPUFEATURE_PSN         0x112    /* Processor Serial Number */
#define CPUFEATURE_CLFLUSH     0x113    /* CLFLUSH Instruction */
#define CPUFEATURE_DS          0x115    /* Debug Store */
#define CPUFEATURE_ACPI        0x116    /* Thermal Monitor and Clock Control */
#define CPUFEATURE_MMX         0x117    /* MMX Technology */
#define CPUFEATURE_FXSR        0x118    /* FXSAVE and FXRSTOR Instructions */
#define CPUFEATURE_SSE         0x119    /* SSE Instructions */
#define CPUFEATURE_SSE2        0x11A    /* SSE2 Instructions */
#define CPUFEATURE_SS          0x11B    /* Self Snoop */
#define CPUFEATURE_HTT         0x11C    /* Multi-threading */
#define CPUFEATURE_TM          0x11D    /* Thermal Monitor */
#define CPUFEATURE_PBE         0x11F    /* Pending Break Enable */

/* Extended Features (ECX) */
#define CPUFEATURE_LAHF        0x200    /* LAHF/SAHF Instructions */
#define CPUFEATURE_PREFETCHW   0x208    /* PREFETCHW Instruction */

/* Extended Features (EDX) */
#define CPUFEATURE_SYSCALL     0x300    /* SYSCALL/SYSRET */
#define CPUFEATURE_NX          0x314    /* Execute Disable Bit */
#define CPUFEATURE_PDPE1GB     0x31A    /* 1GB Pages */
#define CPUFEATURE_RDTSCP      0x31B    /* RDTSCP Instruction */
#define CPUFEATURE_LM          0x31D    /* 64-bit Long Mode */

/**
 * CPU feature requirement check - kernel panic if feature not supported
 * @param feature_id Feature to check
 * @param feature_name String name of feature for error message
 */
static inline void require_cpu_feature(int feature_id, const char *feature_name) {
    if (!cpu_has_feature(feature_id)) {
        kernel_panic("Required CPU feature not supported: %s", feature_name);
    }
}

#endif /* _KERNEL_CPU_FEATURES_H */
