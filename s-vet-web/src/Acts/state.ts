import { createAsyncThunk, createSlice, PayloadAction } from "@reduxjs/toolkit";
import { createAct, updateAct } from "../api";
import { searchActsAction, LocalState as SettingsState } from "../Settings";
import { Act, Keyed, Loadable } from "../types";

const initialState = {
  createModalOpen: false,
  createState: "NotRequested" as Loadable<"Ok">,
  selectedAct: undefined as Keyed<Act> | undefined,
  updateState: "NotRequested" as Loadable<"Ok">,
};

export type LocalState = typeof initialState;
type ThunkAPI = {
  state: {
    settings: SettingsState;
    acts: LocalState;
  };
};

export const createActAction = createAsyncThunk<Keyed<Act>, Act, ThunkAPI>(
  "acts/create",
  async (p: Act, thunkAPI) => {
    const res = await createAct(p);
    thunkAPI.dispatch(searchActsAction(thunkAPI.getState().settings.search));
    return res.data;
  },
);

export const updateActAction = createAsyncThunk<
  Keyed<Act>,
  Keyed<Act>,
  ThunkAPI
>("acts/update", async (p: Keyed<Act>, thunkAPI) => {
  const res = await updateAct(p);
  thunkAPI.dispatch(searchActsAction(thunkAPI.getState().settings.search));
  return res.data;
});

const actsSlice = createSlice({
  name: "acts",
  initialState,
  reducers: {
    openNewActModal: (s) => {
      s.createState = "NotRequested";
      s.createModalOpen = true;
    },
    closeActModal: (s) => {
      s.createModalOpen = false;
    },
    openUpdateActModal: (s, a: PayloadAction<Keyed<Act>>) => {
      s.updateState = "NotRequested";
      s.selectedAct = a.payload;
    },
    closeUpdateActModal: (s) => {
      s.selectedAct = undefined;
    },
  },
  extraReducers: (b) =>
    b
      .addCase(createActAction.pending, (s) => {
        s.createState = "Loading";
      })
      .addCase(createActAction.fulfilled, (s) => {
        s.createState = "Ok";
        s.createModalOpen = false;
      })
      .addCase(createActAction.rejected, (s) => {
        s.createState = "NetworkError";
      })
      .addCase(updateActAction.pending, (s) => {
        s.updateState = "Loading";
      })
      .addCase(updateActAction.fulfilled, (s) => {
        s.updateState = "Ok";
        s.selectedAct = undefined;
      })
      .addCase(updateActAction.rejected, (s) => {
        s.updateState = "NetworkError";
      })
      .addCase("logout/pending", () => initialState),
});

export const {
  openNewActModal,
  closeActModal,
  openUpdateActModal,
  closeUpdateActModal,
} = actsSlice.actions;

export default actsSlice.reducer;
