import { Link } from "react-router-dom";
import { Keyed, Owner } from "../types";
import { Descriptions, DescriptionsItem } from "../Descriptions";

const OwnerPreview = ({ owner: o }: { owner: Keyed<Owner> }) => (
  <Descriptions>
    <h2>
      <Link to={`/owners/${o.key}`}>{o.name}</Link>
    </h2>
    <DescriptionsItem label="Addresse Email">
      {o.email ?? "N/A"}
    </DescriptionsItem>
    <DescriptionsItem label="Addresse">{o.address ?? "N/A"}</DescriptionsItem>
    <DescriptionsItem label="Numéro de téléphone">
      {o.phonenumber ?? "N/A"}
    </DescriptionsItem>
  </Descriptions>
);
export default OwnerPreview;
