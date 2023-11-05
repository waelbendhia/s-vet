import { PageHeader } from "antd";
import { goBack } from "connected-react-router";
import { useDispatch } from "react-redux";
import { useIs2Columns } from "./selectors";

const FocusView = ({
  title = "",
  subTitle = "",
  sideContent = null as React.ReactNode,
  children = null as React.ReactNode,
  responsive = true,
  backButton = true,
}) => {
  const dispatch = useDispatch();
  const using2Columns = useIs2Columns();
  const is2Columns = !responsive || using2Columns;

  return (
    <div className="page-root">
      <PageHeader
        onBack={backButton ? () => dispatch(goBack()) : undefined}
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
