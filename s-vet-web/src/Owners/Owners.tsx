import { Button } from "antd";
import { Link } from "react-router-dom";
import { Keyed, Owner, toComponentState } from "../types";
import { openNewOwnerModal, getOwners } from "./state";
import TableView from "../TableView";
import { useAppSelector } from "../hooks";

export const Consultations = () => {
  const { owners, request } = useAppSelector((s) => ({
    owners: toComponentState(s.owners.owners),
    request: s.owners.searchRequest,
  }));

  return (
    <TableView<Keyed<Owner>>
      title="Propriétaires"
      newItem={openNewOwnerModal()}
      newItemLabel="Nouveau propriétaire"
      loading={owners.state === "Loading"}
      data={owners.data.rows}
      total={owners.data.total}
      page={request.page}
      itemsPerPage={request.itemsPerPage}
      searchAction={getOwners}
      columns={[
        { key: "name", title: "Nom", render: (_, o) => o.name },
        {
          key: "email",
          title: "Addresse Email",
          render: (_, o) => o.email,
        },
        {
          key: "phonenumber",
          title: "Numéro de téléphone",
          render: (_, o) => o.phonenumber,
        },
        {
          key: "address",
          title: "Addresse",
          render: (_, o) => o.address,
        },
        {
          key: "actions",
          title: "Actions",
          render: (_, c) => (
            <Button type="link" size="small">
              <Link to={`/owners/${c.key}`}>Voir</Link>
            </Button>
          ),
        },
      ]}
    />
  );
};

export default Consultations;
