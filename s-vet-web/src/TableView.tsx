import { Button, Form, Input, Table, PageHeader } from "antd";
import { useEffect, useState } from "react";
import { useDispatch } from "react-redux";
import { useDebounce } from "./types";
import { AnyAction } from "redux";
import { ColumnsType } from "antd/lib/table";
import Appointments from "./Appointments";
import { useIs2Columns } from "./selectors";
import { SizeType } from "antd/lib/config-provider/SizeContext";

type SearchRequest = {
  page?: number;
  itemsPerPage?: number;
  search?: string;
};

type TableViewProps<T extends object> = {
  title?: string;
  newItem?: AnyAction;
  newItemLabel?: string;
  loading?: boolean;
  data: T[];
  total?: number;
  page?: number;
  itemsPerPage?: number;
  searchAction?: (_: SearchRequest) => {};
  columns?: ColumnsType<T>;
  extraFilters?: React.ReactNode;
  size?: SizeType;
};

function TableView<T extends object>({
  title = "",
  newItem = { type: "noop" } as AnyAction,
  newItemLabel = "",
  loading = false,
  data,
  total = 0,
  page = 0,
  itemsPerPage = 0,
  searchAction = (_: SearchRequest): {} => ({ type: "noop" }),
  columns = [] as ColumnsType<T>,
  extraFilters,
  size,
}: TableViewProps<T>) {
  const dispatch = useDispatch();
  const [search, setSearch] = useState("");
  const debouncedSearch = useDebounce(search, 200);
  const is2Columns = useIs2Columns();

  useEffect(() => {
    dispatch(searchAction({}));
  }, [dispatch, searchAction]);

  useEffect(() => {
    dispatch(searchAction({ search: debouncedSearch, page: 0 }));
  }, [dispatch, debouncedSearch, searchAction]);

  return (
    <div className="table-view-root">
      <PageHeader
        title={title}
        ghost={false}
        extra={[
          <Button onClick={() => dispatch(newItem)} type="primary">
            {newItemLabel}
          </Button>,
        ]}
      />
      <div className="table-root">
        <div className="table-list">
          <Table<T>
            size={size}
            loading={loading}
            dataSource={data}
            scroll={{ x: true }}
            pagination={{
              total,
              current: page + 1,
              pageSize: itemsPerPage,
            }}
            title={() => (
              <div className="search-controls">
                <Form.Item label="Recherche">
                  <Input onChange={(e) => setSearch(e.target.value)} />
                </Form.Item>
                {extraFilters}
              </div>
            )}
            onChange={(p) => {
              dispatch(searchAction({ page: (p.current ?? 1) - 1 }));
            }}
            columns={columns}
          />
        </div>
        {is2Columns && (
          <div className="appointments-list">
            <h2>Rendez vous</h2>
            <Appointments size="small" />
          </div>
        )}
      </div>
    </div>
  );
}

export default TableView;
