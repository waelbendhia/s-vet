import { AutoComplete, Button } from "antd";
import React from "react";
import { searchPets } from "../api";
import { Owned, Keyed, Pet, isOk } from "../types";
import { openNewPetModal } from "./state";
import { useAppDispatch, useAppSelector } from "../hooks";

const petToSelectValue = (pet: Owned<Keyed<Pet>>) => ({
  pet,
  value: pet.key,
  label: `${pet.owner.name} - ${pet.name}`,
});

export type PetSelectProps = {
  onChange?: (value: Owned<Keyed<Pet>>) => void;
};

const PetSelect = ({ onChange }: PetSelectProps) => {
  const { loggedIn, createResult } = useAppSelector((s) => ({
    loggedIn: isOk(s.login.account),
    createResult: s.pets.createState,
  }));

  const dispatch = useAppDispatch();
  const [search, setSearch] = React.useState("");
  const [pets, setPets] = React.useState<
    { pet: Owned<Keyed<Pet>>; value: number; label: string }[]
  >([]);
  const [selectedPet, setSelectedPet] = React.useState<
    Owned<Keyed<Pet>> | undefined
  >(undefined);

  React.useEffect(() => {
    if (search !== "" && loggedIn) {
      searchPets({ search })
        .then((r) => {
          setPets(r.rows.map(petToSelectValue));
        })
        .catch((e) => console.error(e));
    }
  }, [search, loggedIn]);

  React.useEffect(() => {
    if (isOk(createResult)) {
      setSelectedPet(createResult);
    }
  }, [createResult]);

  React.useEffect(() => {
    if (!!onChange && !!selectedPet) {
      onChange(selectedPet);
    }
  }, [selectedPet, onChange]);

  return (
    <div className="s-vet-autocomplete">
      <AutoComplete
        onSearch={setSearch}
        value={
          !!selectedPet
            ? `${selectedPet.owner.name} - ${selectedPet.name}`
            : undefined
        }
        onSelect={(_, p) => setSelectedPet(p.pet)}
        onDeselect={() => setSelectedPet(undefined)}
        options={pets}
      />
      <Button onClick={() => dispatch(openNewPetModal())}>
        nouveau patient
      </Button>
    </div>
  );
};

export default PetSelect;
