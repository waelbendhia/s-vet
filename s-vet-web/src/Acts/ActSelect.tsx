import { Button, Select } from "antd";
import React, { useEffect } from "react";
import { useDispatch, useSelector } from "react-redux";
import { getActs } from "../api";
import Price from "../Price";
import {
  Keyed,
  isOk,
  Act,
  AppliedTreatment,
  eqAppliedTreatment,
  treatmentName,
} from "../types";
import { openNewActModal } from "./state";

const actToSelectValue = (act: Keyed<Act>) => ({
  act,
  value: act.name,
  label: (
    <>
      {act.name} <Price price={act.price} />
    </>
  ),
});

export type ActsSelectProps = {
  onChange?: (value: AppliedTreatment[]) => void;
  defaultValue?: AppliedTreatment[];
  value?: AppliedTreatment[];
};

const ActSelect = ({ onChange, defaultValue, value }: ActsSelectProps) => {
  const loggedIn = useSelector((s) => isOk(s.login.account));
  const dispatch = useDispatch();
  const [acts, setActs] = React.useState<
    { act: Keyed<Act>; value: string; label: React.ReactNode }[]
  >([]);

  const [selectedActs, setSelectedActs] = React.useState<AppliedTreatment[]>(
    value ?? defaultValue ?? []
  );

  const updateActs = () => {
    if (loggedIn) {
      getActs({})
        .then((v) => setActs(v.map(actToSelectValue)))
        .catch(console.error);
    }
  };

  useEffect(updateActs, [loggedIn]);
  useEffect(() => {
    if (!!onChange) {
      onChange(selectedActs);
    }
  }, [onChange, selectedActs]);

  return (
    <div className="s-vet-autocomplete">
      <Select
        mode="tags"
        defaultValue={defaultValue?.map(treatmentName)}
        value={value?.map(treatmentName)}
        options={acts}
        onSelect={(v, o) => {
          setSelectedActs((as) => {
            const newAct: AppliedTreatment = {
              quantity: 1,
              treatment: !!o.act
                ? { tag: "Specific", contents: o.act }
                : { tag: "General", contents: typeof v === "string" ? v : "" },
            };
            return [...as, newAct];
          });
        }}
        onDeselect={(v, o) => {
          setSelectedActs((as) => {
            const newAct: AppliedTreatment = {
              quantity: 1,
              treatment: !!o.act
                ? { tag: "Specific", contents: o.act }
                : { tag: "General", contents: typeof v === "string" ? v : "" },
            };
            return as.filter((a) => !eqAppliedTreatment(a, newAct));
          });
        }}
        onKeyDown={updateActs}
      />
      <Button onClick={() => dispatch(openNewActModal())}>nouvelle acte</Button>
    </div>
  );
};

export default ActSelect;
