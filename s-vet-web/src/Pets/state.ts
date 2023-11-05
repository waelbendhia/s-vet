import { createAsyncThunk, createSlice } from '@reduxjs/toolkit';
import { DefaultRootState } from 'react-redux';
import { createPet, getPet, searchPets, updatePet } from '../api';
import { logout } from "../Login/state";
import {
  Pet,
  Loadable,
  Owned,
  SearchRequest,
  Keyed,
  SearchResult,
  initMemo,
  WithMemo,
  FullPet,
} from '../types';

export const createPetAction = createAsyncThunk(
  'pets/createPet',
  async (p: Owned<Pet>, thunkAPI) => {
    const res = await createPet(p);
    thunkAPI.dispatch(getPets({}) as any);
    return res;
  }
);

export const getPets = createAsyncThunk<
  SearchResult<Keyed<Owned<Pet>>>,
  SearchRequest,
  { state: DefaultRootState }
>('pets/getPets', (p: SearchRequest, thunkAPI) => {
  const req: SearchRequest = {
    ...thunkAPI.getState().pets.searchRequest,
    ...p,
  };

  return searchPets(req);
});

export const selectPetAction = createAsyncThunk(
  'pets/selectPet',
  (pKey: number) => getPet(pKey)
);

export const updatePetAction = createAsyncThunk('pet/updatePet', updatePet);

const initialState = {
  createModalOpen: false,
  createState: 'NotRequested' as Loadable<Owned<Keyed<Pet>>>,
  searchRequest: { page: 0, itemsPerPage: 10 } as SearchRequest,
  pets: initMemo({ rows: [], total: 0 }) as WithMemo<
    SearchResult<Keyed<Owned<Pet>>>
  >,
  current: 'NotRequested' as Loadable<FullPet>,
  updateState: 'NotRequested' as Loadable<'Ok'>,
};

const petsSlice = createSlice({
  name: 'pets',
  initialState,
  reducers: {
    openNewPetModal: (s) => {
      s.createState = 'NotRequested';
      s.createModalOpen = true;
    },
    closePetModal: (s) => {
      s.createModalOpen = false;
    },
  },
  extraReducers: (b) =>
    b
      .addCase(createPetAction.pending, (s) => {
        s.createState = 'Loading';
      })
      .addCase(createPetAction.fulfilled, (s, a) => {
        s.createState = a.payload;
        s.createModalOpen = false;
      })
      .addCase(createPetAction.rejected, (s) => {
        s.createState = 'NetworkError';
      })
      .addCase(getPets.pending, (s, a) => {
        s.searchRequest = { ...s.searchRequest, ...a.meta.arg };
        s.pets.value = 'Loading';
      })
      .addCase(getPets.fulfilled, (s, a) => {
        s.pets.value = a.payload;
        s.pets.memo = a.payload;
      })
      .addCase(getPets.rejected, (s) => {
        s.pets.value = 'NetworkError';
      })
      .addCase(selectPetAction.pending, (s) => {
        s.current = 'Loading';
      })
      .addCase(selectPetAction.fulfilled, (s, a) => {
        s.current = a.payload;
      })
      .addCase(selectPetAction.rejected, (s) => {
        s.current = 'NetworkError';
      })
      .addCase(updatePetAction.pending, (s) => {
        s.updateState = 'Loading';
      })
      .addCase(updatePetAction.fulfilled, (s) => {
        s.updateState = 'Ok';
      })
      .addCase(updatePetAction.rejected, (s) => {
        s.updateState = 'NetworkError';
      })
      .addCase('logout/pending', () => initialState),
});

export const { openNewPetModal, closePetModal } = petsSlice.actions;

export default petsSlice.reducer;
