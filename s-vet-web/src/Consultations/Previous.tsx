import { Empty } from "antd";
import { Keyed, Consultation } from "../types";
import ConsultationPreview from "./ConsultationPreview";

const Previous = ({ previous }: { previous: Keyed<Consultation>[] }) => (
  <>
    <h2>Consultations précédentes</h2>
    <div className="previous-list">
      {previous.length === 0 ? (
        <Empty description="Aucune consultation précédente" />
      ) : (
        previous.map((p) => (
          <ConsultationPreview consultation={p} key={p.key} />
        ))
      )}
    </div>
  </>
);

export default Previous;
