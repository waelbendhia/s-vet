import { Link } from "react-router-dom";
import { formatAge, Keyed, Pet } from "../types";
import { Descriptions, DescriptionsItem } from "../Descriptions";
import { DateTime } from "luxon";

const PetPreview = ({ pet: p }: { pet: Keyed<Pet> }) => (
  <Descriptions>
    <h2>
      <Link to={`/pets/${p.key}`}>{p.name}</Link>
    </h2>
    <DescriptionsItem label="Age">
      {formatAge(DateTime.fromObject(p.age))}
    </DescriptionsItem>
    <DescriptionsItem label="Espèce">
      {p.species === "Cat" ? "Chat" : "Chien"}
    </DescriptionsItem>
    <DescriptionsItem label="Sexe">
      {p.sex === "Male" ? "Male" : "Femelle"}
    </DescriptionsItem>
  </Descriptions>
);

export default PetPreview;
