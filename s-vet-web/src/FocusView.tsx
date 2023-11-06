import { PageHeader } from "antd";
import { useIs2Columns } from "./selectors";
import { useNavigate } from "react-router-dom";

const FocusView = ({
  title = "",
  subTitle = "",
  sideContent = null as React.ReactNode,
  children = null as React.ReactNode,
  responsive = true,
  backButton = true,
}) => {
  const navigate = useNavigate();
  const using2Columns = useIs2Columns();
  const is2Columns = !responsive || using2Columns;

  return (
    <div className="page-root">
      <PageHeader
        onBack={backButton ? () => navigate(-1) : undefined}
        title={title}
        subTitle={!is2Columns ? subTitle : undefined}
        ghost={false}
      />
      <div className={`focus-root ${!responsive ? "static" : "responsive"}`}>
        <div className="main-content">{children}</div>
        {is2Columns && <div className="side-content">{sideContent}</div>}
      </div>
    </div>
  );
};

export default FocusView;
