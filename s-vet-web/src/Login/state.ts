import { createAsyncThunk, createSlice } from "@reduxjs/toolkit";
import axios, { AxiosError } from "axios";
import { getSettingsAction } from "../Settings";
import { Loadable } from "../types";

export type Account = { email: string } | "Outsider";

export const submitLogin = createAsyncThunk(
  "submitLogin",
  async (login: { email: string; password: string }, thunkAPI) => {
    try {
      const res = await axios.post<Account>("/api/login", login);
      if (typeof res.data === "object") {
        thunkAPI.dispatch(getSettingsAction());
      }
      return res.data;
    } catch (e) {
      return thunkAPI.rejectWithValue(e);
    }
  },
);

export const getAccount = createAsyncThunk(
  "getAccount",
  async (_, thunkAPI) => {
    try {
      const { data } = await axios.get<Account>("/api/account");
      if (typeof data === "object") {
        thunkAPI.dispatch(getSettingsAction());
      }
      return data;
    } catch (e) {
      return thunkAPI.rejectWithValue(e);
    }
  },
);

export const logout = createAsyncThunk("logout", () =>
  axios.post("/api/logout"),
);

const isAxiosError = (x: unknown): x is AxiosError =>
  x instanceof Object && x !== null && x.hasOwnProperty("isAxiosError");

const initialState = {
  account: "NotRequested" as Loadable<Account>,
  loginResult: "NotRequested" as Loadable<"AuthenticationError">,
};

const loginSlice = createSlice({
  name: "login",
  initialState,
  reducers: {},
  extraReducers: (b) =>
    b
      .addCase(getAccount.pending.type, (s) => {
        s.account = "Loading";
      })
      .addCase(submitLogin.pending.type, (s) => {
        s.loginResult = "Loading";
      })
      .addCase(getAccount.fulfilled, (s, a) => {
        s.account = a.payload;
      })
      .addCase(submitLogin.fulfilled, (s, a) => {
        s.account = a.payload;
        s.loginResult =
          typeof a.payload === "object"
            ? "NotRequested"
            : "AuthenticationError";
      })
      .addCase(getAccount.rejected, (s, a) => {
        s.account =
          isAxiosError(a.payload) && a.payload.response?.status === 401
            ? "Outsider"
            : "NetworkError";
      })
      .addCase(submitLogin.rejected, (s, a) => {
        s.loginResult =
          isAxiosError(a.payload) && a.payload.response?.status === 401
            ? "AuthenticationError"
            : "NetworkError";
      })
      .addCase("logout/pending", (s) => {
        s.account = "Loading";
      })
      .addCase(logout.fulfilled, () => initialState)
      .addCase(logout.rejected, () => initialState)
      .addMatcher(
        (a) =>
          !!a.error &&
          a.error.message === "Request failed with status code 401",
        (s) => {
          s.account = "Outsider";
        },
      ),
});

export default loginSlice.reducer;
