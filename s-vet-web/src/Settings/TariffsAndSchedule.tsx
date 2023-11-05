import { Button, Form, InputNumber, Spin, TimePicker } from "antd";
import moment from "moment";
import { useEffect } from "react";
import { useDispatch, useSelector } from "react-redux";
import {
  currencyParser,
  daysOfTheWeek,
  isOk,
  mapPartialRecord,
  Settings,
  tndFormat,
  toComponentState,
  translateWeekday,
  WeekDay,
} from "../types";
import { updateSettingsAction } from "./state";

type SettingsForm = {
  prices: Settings["prices"];
  schedule: Partial<Record<WeekDay, [moment.Moment, moment.Moment]>>;
};

const toSettingsForm = (s: Settings): SettingsForm => ({
  prices: {
    consultation: s.prices.consultation / 1000,
    emergency: s.prices.emergency / 1000,
  },
  schedule: mapPartialRecord(s.schedule, (v) => [
    moment(v.start),
    moment(v.end),
  ]),
});

const fromSettingsForm = (s: SettingsForm): Settings => ({
  prices: {
    consultation: Math.trunc(s.prices.consultation * 1000),
    emergency: Math.trunc(s.prices.emergency * 1000),
  },
  schedule: mapPartialRecord(s.schedule, (v) => ({
    start: {
      h: v[0].get("hour"),
      m: v[0].get("minute"),
      s: v[0].get("seconds"),
    },
    end: {
      h: v[1].get("hour"),
      m: v[1].get("minute"),
      s: v[1].get("seconds"),
    },
  })),
});

const TariffsAndSchedule = () => {
  const dispatch = useDispatch();
  const {
    settings: { state: settingsState, data: settings },
    updateState,
  } = useSelector((s) => ({
    settings: toComponentState(s.settings.settings),
    updateState: s.settings.updateState,
  }));
  const [form] = Form.useForm();

  useEffect(() => {
    if (isOk(settingsState)) {
      form.resetFields();
    }
  }, [form, settingsState]);

  return (
    <div className="tariffs">
      <h2>Tarifs et horaires</h2>
      <Spin spinning={settingsState === "Loading" || updateState === "Loading"}>
        <h3>Tarifs</h3>
        <Form<SettingsForm>
          form={form}
          initialValues={toSettingsForm(settings)}
          onFinish={(v) => dispatch(updateSettingsAction(fromSettingsForm(v)))}
        >
          <div className="form-row-2">
            <Form.Item name={["prices", "consultation"]} label="Consultation">
              <InputNumber<number>
                formatter={(v) => tndFormat.format(v ?? 0)}
                parser={(v) => (!!v ? currencyParser(v) : 0)}
              />
            </Form.Item>
            <Form.Item
              name={["prices", "emergency"]}
              label="Consultation d'urgence"
            >
              <InputNumber<number>
                formatter={(v) => tndFormat.format(v ?? 0)}
                parser={(v) => (!!v ? currencyParser(v) : 0)}
              />
            </Form.Item>
          </div>
          <h3>Horaires</h3>
          <div className="schedule">
            {daysOfTheWeek.map((w) => {
              const label = translateWeekday(w);
              return (
                <Form.Item
                  key={`schedule.${w}`}
                  name={["schedule", w]}
                  label={label.charAt(0).toUpperCase() + label.slice(1)}
                >
                  <TimePicker.RangePicker />
                </Form.Item>
              );
            })}
          </div>
          <Form.Item style={{ marginBottom: 0 }}>
            <Button
              type="primary"
              htmlType="submit"
              loading={settingsState === "Loading" || updateState === "Loading"}
            >
              Confirmer
            </Button>
          </Form.Item>
        </Form>
      </Spin>
    </div>
  );
};

export default TariffsAndSchedule;
