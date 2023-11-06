import { createAsyncThunk, createSlice } from "@reduxjs/toolkit";
import { DateTime } from "luxon";
import { getConsultationsByHour } from "../api";
import { initMemo, ConsultationsByHour } from "../types";

type DateRange = { after?: string; before?: string };

export const defaultSearch = () => {
  const now = DateTime.local();
  return {
    after: now.startOf("week").toFormat("yyyy-MM-dd"),
    before: now.endOf("week").toFormat("yyyy-MM-dd"),
  };
};

const initialState = {
  consultationsByHour: initMemo<ConsultationsByHour>({}),
  searchRequest: defaultSearch() as DateRange,
};

export type LocalState = typeof initialState;
type ThunkAPI = { state: { dashboard: LocalState } };

export const getConsultationsByHourAction = createAsyncThunk<
  ConsultationsByHour,
  DateRange,
  ThunkAPI
>("dashboard/getConsultationsByHour", (p: DateRange, thunkAPI) => {
  const req: DateRange = thunkAPI.getState().dashboard.searchRequest;
  return getConsultationsByHour({ ...req, ...p });
});

const dashboardSlice = createSlice({
  name: "dashboard",
  initialState,
  reducers: {},
  extraReducers: (b) =>
    b
      .addCase(getConsultationsByHourAction.pending, (s, a) => {
        s.searchRequest = { ...s.searchRequest, ...a.meta.arg };
        s.consultationsByHour.value = "Loading";
      })
      .addCase(getConsultationsByHourAction.fulfilled, (s, a) => {
        s.consultationsByHour.value = a.payload;
        s.consultationsByHour.memo = a.payload;
      })
      .addCase(getConsultationsByHourAction.rejected, (s) => {
        s.consultationsByHour.value = "NetworkError";
      })
      .addCase("logout/pending", () => initialState),
});

export default dashboardSlice.reducer;
