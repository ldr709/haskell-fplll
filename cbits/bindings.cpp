#include <gmp.h>
#include <fplll/defs.h>
#include <fplll/wrapper.h>

using namespace fplll;

extern "C"
{
	RedStatus hs_ffi_lll_reduction(int vecs, int len, mpz_t* b, double delta, double eta,
	                               LLLMethod method, FloatType floatType, int precision,
	                               int flags);
	RedStatus hs_ffi_lll_reduction_u(int vecs, int len, mpz_t* b, int u_len, mpz_t* u, double delta,
	                                 double eta, LLLMethod method, FloatType floatType,
	                                 int precision, int flags);
	RedStatus hs_ffi_lll_reduction_uinv(int vecs, int len, mpz_t* b, int u_len, mpz_t* u,
	                                    mpz_t* u_inv, double delta, double eta, LLLMethod method,
	                                    FloatType floatType, int precision, int flags);

	extern const int lmWrapper   = LM_WRAPPER;
	extern const int lmProved    = LM_PROVED;
	extern const int lmHeuristic = LM_HEURISTIC;
	extern const int lmFast      = LM_FAST;

	extern const char* const* const lllMethodStr = LLL_METHOD_STR;

	extern const int ftDefault    = FT_DEFAULT;
	extern const int ftDouble     = FT_DOUBLE;
	extern const int ftLongDouble = FT_LONG_DOUBLE;
	extern const int ftDpe        = FT_DPE;
	extern const int ftDD         = FT_DD;
	extern const int ftQD         = FT_QD;
	extern const int ftMpfr       = FT_MPFR;

	extern const char* const* const floatTypeStr = FLOAT_TYPE_STR;

	extern const int redSuccess         = RED_SUCCESS;
	extern const int redGsoFailure      = RED_GSO_FAILURE;
	extern const int redBabaiFailure    = RED_BABAI_FAILURE;
	extern const int redLllFailure      = RED_LLL_FAILURE;
	extern const int redEnumFailure     = RED_ENUM_FAILURE;
	extern const int redBkzFailure      = RED_BKZ_FAILURE;
	extern const int redBkzTimeLimit    = RED_BKZ_TIME_LIMIT;
	extern const int redBkzLoopsLimit   = RED_BKZ_LOOPS_LIMIT;
	extern const int redHlllFailure     = RED_HLLL_FAILURE;
	extern const int redHlllNormFailure = RED_HLLL_NORM_FAILURE;
	extern const int redHlllSrFailure   = RED_HLLL_SR_FAILURE;

	extern const char* const* const redStatusStr = RED_STATUS_STR;
}

static ZZ_mat<mpz_t> construct_zzmat(int vecs, int len, const mpz_t* in)
{
	ZZ_mat<mpz_t> out(vecs, len);
	for (int i = 0; i < vecs; i++)
		for (int j = 0; j < len; j++)
			out(i, j) = in[i*len + j];
	return out;
}

static void destructure_zzmat(int vecs, int len, const ZZ_mat<mpz_t>& in,
                              mpz_t* out)
{
	for (int i = 0; i < vecs; i++)
		for (int j = 0; j < len; j++)
			in(i, j).get_mpz(out[i*len + j]);
}

RedStatus hs_ffi_lll_reduction(int vecs, int len, mpz_t* b, double delta, double eta,
                               LLLMethod method, FloatType floatType, int precision, int flags)
{
	ZZ_mat<mpz_t> b_zzmat = construct_zzmat(vecs, len, b);

	RedStatus exit_code = (RedStatus) lll_reduction(
		b_zzmat, delta, eta, method, floatType, precision, flags);
	if (exit_code != RED_SUCCESS)
		return exit_code;

	destructure_zzmat(vecs, len, b_zzmat, b);
	return exit_code;
}

RedStatus hs_ffi_lll_reduction_u(int vecs, int len, mpz_t* b, int u_len, mpz_t* u, double delta,
                                 double eta, LLLMethod method, FloatType floatType,
                                 int precision, int flags)
{
	ZZ_mat<mpz_t> b_zzmat = construct_zzmat(vecs, len, b);
	ZZ_mat<mpz_t> u_zzmat = construct_zzmat(vecs, u_len, u);

	RedStatus exit_code = (RedStatus) lll_reduction(
		b_zzmat, u_zzmat, delta, eta, method, floatType, precision, flags);
	if (exit_code != RED_SUCCESS)
		return exit_code;

	destructure_zzmat(vecs, len, b_zzmat, b);
	destructure_zzmat(vecs, u_len, u_zzmat, u);
	return exit_code;
}

RedStatus hs_ffi_lll_reduction_uinv(int vecs, int len, mpz_t* b, int u_len, mpz_t* u,
                                    mpz_t* u_inv, double delta, double eta, LLLMethod method,
                                    FloatType floatType, int precision, int flags)
{
	ZZ_mat<mpz_t> b_zzmat = construct_zzmat(vecs, len, b);
	ZZ_mat<mpz_t> u_zzmat = construct_zzmat(vecs, u_len, u);
	ZZ_mat<mpz_t> uinv_zzmat;
	uinv_zzmat.gen_identity(vecs);

	RedStatus exit_code = (RedStatus) lll_reduction(
		b_zzmat, u_zzmat, uinv_zzmat, delta, eta, method, floatType, precision, flags);
	if (exit_code != RED_SUCCESS)
		return exit_code;

	destructure_zzmat(vecs, len, b_zzmat, b);
	destructure_zzmat(vecs, u_len, u_zzmat, u);
	destructure_zzmat(vecs, vecs, uinv_zzmat, u_inv);
	return exit_code;
}
