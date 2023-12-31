import { AutoComplete, Button } from "antd";
import React from "react";
import { searchOwners } from "../api";
import { Keyed, Owner, isOk } from "../types";
import { openNewOwnerModal } from "./state";
import { useAppDispatch, useAppSelector } from "../hooks";

const ownerToSelectValue = (owner: Keyed<Owner>) => ({
  owner,
  value: owner.name,
  label: owner.name,
});

export type OwnerSelectProps = {
  onChange?: (value: Keyed<Owner>) => void;
};

const OwnerSelect = ({ onChange }: OwnerSelectProps) => {
  const { loggedIn, createResult } = useAppSelector((s) => ({
    loggedIn: isOk(s.login.account),
    createResult: s.owners.createState,
  }));

  const dispatch = useAppDispatch();
  const [search, setSearch] = React.useState("");
  const [owners, setOwners] = React.useState<
    { owner: Keyed<Owner>; value: string; label: string }[]
  >([]);
  const [selectedOwner, setSelectedOwner] = React.useState<
    Keyed<Owner> | undefined
  >(undefined);

  React.useEffect(() => {
    if (search !== "" && loggedIn) {
      searchOwners({ search })
        .then((r) => {
          setOwners(r.rows.map(ownerToSelectValue));
        })
        .catch((e) => console.error(e));
    }
  }, [search, loggedIn]);

  React.useEffect(() => {
    if (isOk(createResult)) {
      setSelectedOwner(createResult);
    }
  }, [createResult]);

  React.useEffect(() => {
    if (!!onChange && !!selectedOwner) {
      onChange(selectedOwner);
    }
  }, [selectedOwner, onChange]);

  return (
    <div className="s-vet-autocomplete">
      <AutoComplete
        onSearch={setSearch}
        value={selectedOwner?.name}
        onSelect={(_, p) => setSelectedOwner(p.owner)}
        onDeselect={() => setSelectedOwner(undefined)}
        options={owners}
      />
      <Button onClick={() => dispatch(openNewOwnerModal())}>
        nouveau propriétaire
      </Button>
    </div>
  );
};

export default OwnerSelect;
