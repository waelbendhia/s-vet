import { createAsyncThunk, createSlice } from '@reduxjs/toolkit';
import { DateTime } from 'luxon';
import { searchConsultations, getConsultationStatistics } from '../api';
import { logout } from '../Login/state';
import {
  ConsultationStatistics,
  FullConsultation,
  initMemo,
  WithMemo,
} from '../types';

export const getHomeData = createAsyncThunk<
  {
    statistics: ConsultationStatistics;
    consultations: FullConsultation[];
  },
  void,
  { state: { home: { consultations: WithMemo<any> } } }
>(
  'getHomeData',
  async () => {
    const now = DateTime.local();
    const d = await searchConsultations({
      after: now.set({ hour: 0, minute: 0, second: 0, millisecond: 0 }),
      before: now.set({ hour: 24, minute: 0, second: 0, millisecond: 0 }),
      status: 'pending',
    });
    const statistics = await getConsultationStatistics();
    return { statistics, consultations: d.rows };
  },
  {
    condition: (_, { getState }) => {
      const { home } = getState();
      return home.consultations.value !== 'Loading';
    },
  }
);

  const initialState= {
    consultations: initMemo<FullConsultation[]>([]),
    statistics: initMemo<ConsultationStatistics>({
      remainingConsultations: 0,
      doneConsultations: 0,
      earnings: 0,
    }),
  }

const homeSlice = createSlice({
  name: 'home',
  initialState ,
  reducers: {},
  extraReducers: (b) =>
    b
      .addCase(getHomeData.pending, (s) => {
        s.consultations.value = 'Loading';
        s.statistics.value = 'Loading';
      })
      .addCase(getHomeData.fulfilled, (s, a) => {
        s.consultations.value = a.payload.consultations;
        s.consultations.memo = a.payload.consultations;
        s.statistics.value = a.payload.statistics;
        s.statistics.memo = a.payload.statistics;
      })
      .addCase(getHomeData.rejected, (s) => {
        s.consultations.value = 'NetworkError';
        s.statistics.value = 'NetworkError';
      })
      .addCase('logout/pending', () => initialState),
});

export default homeSlice.reducer;
