import { AutoComplete, Button } from "antd";
import React from "react";
import { useDispatch, useSelector } from "react-redux";
import { searchOwners } from "../api";
import { Keyed, Owner, isOk } from "../types";
import { openNewOwnerModal } from "./state";

const ownerToSelectValue = (owner: Keyed<Owner>) => ({
  owner,
  value: owner.name,
  label: owner.name,
});

export type OwnerSelectProps = {
  onChange?: (value: Keyed<Owner>) => void;
};

const OwnerSelect = ({ onChange }: OwnerSelectProps) => {
  const loggedIn = useSelector((s) => isOk(s.login.account));
  const createResult = useSelector((s) => s.owners.createState);

  const dispatch = useDispatch();
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
