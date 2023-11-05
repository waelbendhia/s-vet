import { createAsyncThunk, createSlice } from '@reduxjs/toolkit';
import { DateTime } from 'luxon';
import { DefaultRootState } from 'react-redux';
import { getConsultationsByHour } from '../api';
import { logout } from '../Login/state';
import { initMemo, ConsultationsByHour } from '../types';

type DateRange = { after?: string; before?: string };

export const getConsultationsByHourAction = createAsyncThunk<
  ConsultationsByHour,
  DateRange,
  { state: DefaultRootState }
>('dashboard/getConsultationsByHour', (p: DateRange, thunkAPI) => {
  const req: DateRange = thunkAPI.getState().dashboard.searchRequest;
  return getConsultationsByHour({ ...req, ...p });
});

export const defaultSearch = () => {
  const now = DateTime.local();
  return {
    after: now.startOf('week').toFormat('yyyy-MM-dd'),
    before: now.endOf('week').toFormat('yyyy-MM-dd'),
  };
};

const initialState = {
  consultationsByHour: initMemo<ConsultationsByHour>({}),
  searchRequest: defaultSearch() as DateRange,
};

const dashboardSlice = createSlice({
  name: 'dashboard',
  initialState,
  reducers: {},
  extraReducers: (b) =>
    b
      .addCase(getConsultationsByHourAction.pending, (s, a) => {
        s.searchRequest = { ...s.searchRequest, ...a.meta.arg };
        s.consultationsByHour.value = 'Loading';
      })
      .addCase(getConsultationsByHourAction.fulfilled, (s, a) => {
        s.consultationsByHour.value = a.payload;
        s.consultationsByHour.memo = a.payload;
      })
      .addCase(getConsultationsByHourAction.rejected, (s) => {
        s.consultationsByHour.value = 'NetworkError';
      })
      .addCase('logout/pending', () => initialState),
});

export default dashboardSlice.reducer;
