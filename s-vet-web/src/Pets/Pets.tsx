import { Button } from "antd";
import { Link } from "react-router-dom";
import { formatAge, Keyed, Owned, Pet, toComponentState } from "../types";
import { openNewPetModal, getPets } from "./state";
import TableView from "../TableView";
import { DateTime } from "luxon";
import { useAppSelector } from "../hooks";

export const Pets = () => {
  const { pets, request } = useAppSelector((s) => ({
    pets: toComponentState(s.pets.pets),
    request: s.pets.searchRequest,
  }));

  return (
    <TableView<Keyed<Owned<Pet>>>
      title="Patients"
      newItem={openNewPetModal()}
      newItemLabel="Nouveau Patient"
      loading={pets.state === "Loading"}
      data={pets.data.rows}
      total={pets.data.total}
      page={request.page}
      itemsPerPage={request.itemsPerPage}
      searchAction={getPets}
      columns={[
        { key: "name", title: "Nom", render: (_, p) => p.name },
        {
          key: "age",
          title: "Age",
          render: (_, p) => formatAge(DateTime.fromObject(p.age)),
        },
        {
          key: "species",
          title: "Espèce",
          render: (_, p) => (p.species === "Cat" ? "Chat" : "Dog"),
        },
        {
          key: "sex",
          title: "Sexe",
          render: (_, p) => (p.sex === "Male" ? "Male" : "Femelle"),
        },
        {
          key: "breed",
          title: "Race",
          render: (_, p) => p.breed ?? "N/A",
        },
        {
          key: "owner.name",
          title: "Propriétaires",
          render: (_, p) => (
            <Link to={`/owners/${p.owner.key}`}>{p.owner.name}</Link>
          ),
        },
        {
          key: "actions",
          title: "Actions",
          render: (_, c) => (
            <Button type="link" size="small">
              <Link to={`/pets/${c.key}`}>Voir</Link>
            </Button>
          ),
        },
      ]}
    />
  );
};

export default Pets;
