import React, { Component } from 'react';
import { observer } from 'mobx-react';
import {
  Form, Select, Input, DatePicker, Icon,
} from 'choerodon-ui';
import { Modal } from 'choerodon-ui/pro';
import { stores } from '@choerodon/boot';
import { isNull, find } from 'lodash';
import originMoment from 'moment';
import { extendMoment } from 'moment-range';

// @ts-ignore

import BacklogStore from '@/stores/project/backlog/BacklogStore';
import { sprintApi } from '@/api';
import WorkCalendar from '@/components/WorkCalendar';
import useIsInProgram from '@/hooks/useIsInProgram';

const moment = extendMoment(originMoment);
const FormItem = Form.Item;
const { TextArea } = Input;
const { Option } = Select;
const { AppState } = stores;
const format = 'YYYY-MM-DD';

@observer
class StartSprint extends Component {
  constructor(props) {
    super(props);
    const { data } = props;
    this.state = {
      startDate: data.startDate,
      endDate: data.endDate,
      showCalendar: false,
      workDates: [], // 冲刺自定义设置
    };
  }

  componentDidMount() {
    const { modal } = this.props;
    modal.handleOk(this.handleStartSprint);
  }

  /**
   *开启冲刺事件
   *
    * @memberof StartSprint
   */
  handleStartSprint = () => {
    const { workDates } = this.state;
    const {
      form, data, modal, isShowFeature,
    } = this.props;
    form.validateFields((err, values) => {
      if (!err) {
        const newData = {
          endDate: values.endDate ? `${moment(values.endDate).format('YYYY-MM-DD HH:mm:ss')}` : null,
          startDate: values.startDate ? `${moment(values.startDate).format('YYYY-MM-DD HH:mm:ss')}` : null,
          projectId: AppState.currentMenuType.id,
          sprintGoal: values.goal,
          sprintId: data.sprintId,
          sprintName: values.name,
          objectVersionNumber: data.objectVersionNumber,
          workDates,
        };
        sprintApi.start(newData, isShowFeature).then((res) => {
          modal.close();
          BacklogStore.refresh();
        }).catch(() => {
        });
      }
    });
    return false;
  };

  showWorkCalendar = () => {
    const { showCalendar } = this.state;
    this.setState({ showCalendar: !showCalendar });
  };

  getWorkDays = (startDate, endDate) => {
    if (moment(startDate).isAfter(moment(endDate))) {
      return 0;
    }
    // 是否显示非工作日
    const { workSetting } = this.props;
    const {
      saturdayWork,
      sundayWork,
      useHoliday,
      timeZoneWorkCalendarDTOS: selectDays,
      workHolidayCalendarDTOS: holidayRefs,
    } = workSetting;
    const { workDates } = this.state;
    // 判断日期是否是休息日
    // 优先级如下：
    // 冲刺自定义设置
    // 组织自定义设置
    // 法定工作日
    // 周六周日是否工作

    const isRestDay = (date) => {
      // status为0代表放假，为1代表上班。
      const sprintSetting = find(workDates, (c) => c.workDay === date.format('YYYY-MM-DD'));
      if (sprintSetting) {
        if (sprintSetting.status === 0) {
          return true;
        }
        return false;
      }
      const orgSetting = find(selectDays, (c) => c.workDay === date.format('YYYY-MM-DD'));
      if (orgSetting) {
        if (orgSetting.status === 0) {
          return true;
        }
        return false;
      }
      // 法定节假日
      const holidayConfig = find(holidayRefs, (c) => c.holiday === date.format('YYYY-MM-DD'));
      if (holidayConfig) {
        if (holidayConfig.status === 0) {
          return true;
        }
        return false;
      }
      const isSaturday = date.weekday() === 5;
      const isSunday = date.weekday() === 6;
      if (isSaturday) {
        return !saturdayWork;
      }
      if (isSunday) {
        return !sundayWork;
      }

      return false;
    };
    const range = moment.range(startDate, endDate);
    const days = Array.from(range.by('day'));
    return days.filter((date) => !isRestDay(date)).length;
  };

  onWorkDateChange = (workDates) => {
    this.setState({
      workDates,
    });
  };

  isDisabledOption(value) {
    const { data: { sprintType } } = this.props;
    if (sprintType) {
      const { data: { sprintId } } = this.props;
      // 开启日期是当前时间
      const startDate = moment();
      // 使用moment套一下，因为add是会改变原值的
      const endDate = moment(startDate).add(parseInt(value, 10), 'weeks');
      return !BacklogStore.rangeCanChoose({ startDate, endDate, sprintId });
    }
    return false;
  }

  render() {
    const {
      data,
      data: { sprintId },
      workSetting,
      sprintDetail,
      form,
      form: { getFieldDecorator, getFieldValue, setFieldsValue },
      isShowFeature,
    } = this.props;

    const {
      showCalendar,
      startDate,
      endDate,
    } = this.state;
    const {
      piId, startDate: start, endDate: end, sprintType,
    } = data;
    const {
      saturdayWork,
      sundayWork,
      useHoliday,
      timeZoneWorkCalendarDTOS: selectDays,
      workHolidayCalendarDTOS: holidayRefs,
    } = workSetting;
    return (
      <div>
        <p className="c7n-closeSprint-message">
          {`该冲刺中包含了${!isNull(sprintDetail) ? sprintDetail.issueCount : 0}个问题`}
        </p>
        <Form style={{ width: 512, marginTop: 24 }}>
          <FormItem>
            {getFieldDecorator('name', {
              initialValue: !isNull(sprintDetail) ? sprintDetail.sprintName : null,
              rules: [{
                required: true,
                message: '冲刺名称是必填的',
              }],
            })(
              <Input label="Sprint名称" maxLength={30} disabled={!!data.piId} />,
            )}
          </FormItem>
          <FormItem>
            {getFieldDecorator('goal', {
              initialValue: !isNull(sprintDetail) ? sprintDetail.sprintGoal : null,
            })(
              <TextArea label="目标" autoSize maxLength={30} />,
            )}
          </FormItem>
          <FormItem>
            {getFieldDecorator('duration', {
              initialValue: '0',
            })(
              <Select
                label="周期"
                onChange={(value) => {
                  if (parseInt(value, 10) > 0) {
                    if (!getFieldValue('startDate')) {
                      setFieldsValue({
                        startDate: moment(),
                      });
                      this.setState({
                        startDate: moment(),
                      });
                    }
                    setFieldsValue({
                      endDate: moment(getFieldValue('startDate')).add(parseInt(value, 10), 'w'),
                    });
                    this.setState({
                      endDate: moment(getFieldValue('startDate')).add(parseInt(value, 10), 'w'),
                    });
                  }
                }}
              >
                <Option value="0">自定义</Option>
                <Option value="1" disabled={this.isDisabledOption('1')}>1周</Option>
                <Option value="2" disabled={this.isDisabledOption('2')}>2周</Option>
                <Option value="4" disabled={this.isDisabledOption('4')}>4周</Option>
              </Select>,
            )}
          </FormItem>
          {sprintType
            ? (
              <FormItem>
                {getFieldDecorator('startDate', {
                  rules: [{
                    required: true,
                    message: '开始日期是必填的',
                  }],
                  initialValue: moment(),
                })(
                  <DatePicker
                    style={{ width: '100%' }}
                    label="开始日期"
                    showTime
                    format="YYYY-MM-DD HH:mm:ss"
                    disabled
                  />,
                )}
              </FormItem>
            ) : (
              <FormItem>
                {getFieldDecorator('startDate', {
                  rules: [{
                    required: true,
                    message: '开始日期是必填的',
                  }],
                  initialValue: start ? (() => {
                    if (isShowFeature) {
                      return moment();
                    }
                    return moment(start);
                  })() : undefined,
                })(
                  <DatePicker
                    style={{ width: '100%' }}
                    label="开始日期"
                    showTime
                    format="YYYY-MM-DD HH:mm:ss"
                    disabledDate={(current) => {
                      if (current < moment()) {
                        return true;
                      }
                      if (endDate && current > moment(endDate)) {
                        return true;
                      }
                      return false;
                    }}
                    onChange={(date) => {
                      setFieldsValue({
                        startDate: date,
                      });
                      this.setState({
                        startDate: date,
                      });
                      if (parseInt(getFieldValue('duration'), 10) > 0) {
                        setFieldsValue({
                          endDate: moment(getFieldValue('startDate')).add(parseInt(getFieldValue('duration'), 10), 'w'),
                        });
                        this.setState({
                          endDate: moment(getFieldValue('startDate')).add(parseInt(getFieldValue('duration'), 10), 'w'),
                        });
                      }
                    }}
                  />,
                )}
              </FormItem>
            )}
          {sprintType
            ? (
              <FormItem>
                {getFieldDecorator('endDate', {
                  rules: [{
                    required: true,
                    message: '结束日期是必填的',
                  }],
                  initialValue: moment(end),
                })(
                  <DatePicker
                    style={{ width: '100%' }}
                    label="结束日期"
                    format="YYYY-MM-DD HH:mm:ss"
                    onChange={(date) => {
                      this.setState({
                        endDate: date,
                      });
                    }}
                    disabled={sprintDetail.type === 'ip' || parseInt(getFieldValue('duration'), 10) > 0}
                    disabledDate={(date) => {
                      if (date < moment()) {
                        return true;
                      }
                      // eslint-disable-next-line no-shadow
                      const startDate = form.getFieldValue('startDate');
                      if (!sprintType && startDate) {
                        return date <= startDate;
                      }
                      // 没选开始时间的时候，只判断时间点能不能选
                      // eslint-disable-next-line no-lonely-if
                      if (!startDate) {
                        return !BacklogStore.dateCanChoose({ date, sprintId });
                      }
                      // 选了开始时间之后，判断形成的时间段是否和其他重叠
                      return !BacklogStore.rangeCanChoose({ startDate, endDate: date, sprintId });
                    }}
                    showTime
                  />,
                )}
              </FormItem>
            ) : (
              <FormItem>
                {getFieldDecorator('endDate', {
                  rules: [{
                    required: true,
                    message: '结束日期是必填的',
                  }],
                  initialValue: end ? moment(end) : undefined,
                })(
                  <DatePicker
                    style={{ width: '100%' }}
                    label="结束日期"
                    format="YYYY-MM-DD HH:mm:ss"
                    showTime
                    onChange={(date) => {
                      this.setState({
                        endDate: date,
                      });
                    }}
                    disabledDate={(current) => {
                      if (current < moment()) {
                        return true;
                      }
                      if (startDate && current < moment(startDate)) {
                        return true;
                      }
                      return false;
                    }}
                  />,
                )}
              </FormItem>
            )}
        </Form>
        {!sprintType && startDate && endDate
          ? (
            <div>
              <div style={{ marginBottom: 20 }}>
                <span style={{ marginRight: 20 }}>
                  {`此Sprint中有${this.getWorkDays(startDate, endDate)}个工作日`}
                </span>
                <Icon type="settings" style={{ verticalAlign: 'top' }} />
                <a onClick={this.showWorkCalendar} role="none">
                  设置当前冲刺工作日
                </a>
              </div>
              {showCalendar
                ? (
                  <WorkCalendar
                    startDate={moment.isMoment(startDate) ? startDate.format(format) : moment(startDate).format(format)}
                    endDate={moment.isMoment(endDate) ? endDate.format(format) : moment(endDate).format(format)}
                    mode="BacklogComponent"
                    saturdayWork={saturdayWork}
                    sundayWork={sundayWork}
                    useHoliday={useHoliday}
                    selectDays={selectDays}
                    holidayRefs={holidayRefs}
                    onWorkDateChange={this.onWorkDateChange}
                  />
                ) : null}
            </div>
          ) : ''}
        {sprintType
          ? (
            <div>
              <div style={{ marginBottom: 20 }}>
                <span style={{ marginRight: 20 }}>
                  {`此Sprint中有${this.getWorkDays(moment(), endDate)}个工作日`}
                </span>
                <Icon type="settings" style={{ verticalAlign: 'top' }} />
                <a onClick={this.showWorkCalendar} role="none">
                  设置当前冲刺工作日
                </a>
              </div>
              {showCalendar
                ? (
                  <WorkCalendar
                    startDate={moment().format(format)}
                    endDate={moment.isMoment(endDate) ? endDate.format(format) : moment(endDate).format(format)}
                    mode="BacklogComponent"
                    saturdayWork={saturdayWork}
                    sundayWork={sundayWork}
                    useHoliday={useHoliday}
                    selectDays={selectDays}
                    holidayRefs={holidayRefs}
                    onWorkDateChange={this.onWorkDateChange}
                  />
                ) : null}
            </div>
          ) : ''}

      </div>
    );
  }
}
const FormStartSprint = Form.create()(StartSprint);

const FormStartSprintHoc = (props) => {
  const { isShowFeature } = useIsInProgram();
  return <FormStartSprint {...props} isShowFeature={isShowFeature} />;
};

export default function (props) {
  Modal.open({
    key: 'sprint',
    title: '开启冲刺',
    okText: '开启',
    cancelText: '取消',
    drawer: true,
    style: {
      width: 740,
    },
    children: <FormStartSprintHoc {...props} />,
  });
}
