import React, { Component, createRef } from 'react';
import moment from 'moment';
import { observer } from 'mobx-react';
import { DatePicker, message, Form } from 'choerodon-ui';
import TextEditToggle from '@/components/TextEditToggle';
import { getProjectId, catchFailed } from '@/common/utils';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import IsInProgramStore from '../../../../../stores/common/program/IsInProgramStore';

const { Text, Edit } = TextEditToggle;
const FormItem = Form.Item;

@observer class SprintDateRange extends Component {
  constructor(props) {
    super(props);
    this.startDateEdit = createRef();
    this.endDateEdit = createRef();
  }

  handleUpdateDate = (date) => {
    // const date = `${dateString} 00:00:00`;
    const { data } = this.props;
    const { startDate, endDate } = date;
    const { objectVersionNumber, sprintId } = data;
    const req = {
      objectVersionNumber,
      projectId: getProjectId(),
      sprintId: data.sprintId,
      startDate,
      endDate,
    };
    BacklogStore.axiosUpdateSprint(req, IsInProgramStore.isShowFeature).then(res => catchFailed(res)).then((res) => {
      BacklogStore.updateSprint(sprintId, {
        objectVersionNumber: res.objectVersionNumber,
        startDate: res.startDate,
        endDate: res.endDate,
      });
      // 在项目群的子项目 刷新冲刺限制列表
      if (IsInProgramStore.isShowFeature) {
        IsInProgramStore.loadPiInfoAndSprint();
      }
    }).catch((error) => {
      message.error(error);
    });
  };

  /**
   * 防止时间相同
   */
  checkDateSame = (name, callback) => {
    const { form } = this.props;
    const formStartDate = form.getFieldValue('startDate');
    const formEndDate = form.getFieldValue('endDate');
    if (formStartDate.isSame(formEndDate)) {
      callback(`${name === 'endDate' ? '结束' : '开始'}时间与${name === 'endDate' ? '开始' : '结束'}时间相同`);
    }
    callback();
  }

  checkEndDate = (rule, value, callback) => {
    const {
      data: { sprintId }, form,
    } = this.props;
    const formStartDate = form.getFieldValue('startDate');

    if (formStartDate && value) {
      this.checkDateSame('endDate', callback);
      const formStartDateFormat = formStartDate.format('YYYY-MM-DD HH:mm:ss');

      const maxTime = IsInProgramStore.findDateMaxRange(formStartDateFormat, sprintId);
      if (moment(value.format('YYYY-MM-DD HH:mm:ss')).isAfter(maxTime)) {
        callback('日期与已存在的冲刺日期有重合');
      }
    }
    callback();
  }

  checkStartDate = (rule, value, callback) => {
    const { data: { sprintId }, form } = this.props;
    const formEndDate = form.getFieldValue('endDate');
    if (formEndDate && value) {
      this.checkDateSame('startDate', callback);
      const formEndDateFormat = formEndDate.format('YYYY-MM-DD HH:mm:ss');
      const minTime = IsInProgramStore.findDateMinRange(formEndDateFormat, sprintId);
      if (moment(value.format('YYYY-MM-DD HH:mm:ss')).isBefore(minTime)) {
        callback('日期与已存在的冲刺日期有重合');
      }
    }
    callback();
  }

  render() {
    const {
      data: {
        statusCode, startDate, endDate, sprintId, sprintType,
      }, disabled, form: { getFieldDecorator }, form,
    } = this.props;
    return (IsInProgramStore.isShowFeature || statusCode === 'started') ? (
      <div
        className="c7n-backlog-sprintData"
        style={{
          display: 'flex',
          alignItems: 'center',
        }}
        role="none"
      >
        <TextEditToggle
          disabled={disabled}
          saveRef={this.startDateEdit}
          onSubmit={() => {
            form.validateFields((err, values) => {
              if (!err) {
                // console.log('v', values);
                this.handleUpdateDate({
                  startDate: values.startDate.format('YYYY-MM-DD HH:mm:ss'),
                  endDate: values.endDate.format('YYYY-MM-DD HH:mm:ss'),
                });
              }
            });
            form.resetFields();
          }}
        >
          <Text>
            <div
              className="c7n-backlog-sprintDataItem"
              role="none"
            >
              {startDate ? moment(startDate, 'YYYY-MM-DD HH:mm:ss').format('YYYY年MM月DD日 HH:mm:ss') : '无'}
            </div>
            <p>~</p>
            <div
              className="c7n-backlog-sprintDataItem"
            >
              {endDate ? moment(endDate, 'YYYY-MM-DD HH:mm:ss').format('YYYY年MM月DD日 HH:mm:ss') : '无'}
            </div>
          </Text>
          <Edit>
            <Form>
              <div style={{ display: 'flex' }}>
                <FormItem>
                  {getFieldDecorator('startDate', {
                    rules: [
                      { required: true, message: '请选择开始日期' },
                      { validator: this.checkStartDate },
                    ],
                    initialValue: startDate ? moment(startDate, 'YYYY-MM-DD HH:mm:ss') : '',
                  })(
                    <DatePicker
                      autoFocus
                      style={{ width: 165, height: 32 }}
                      allowClear
                      disabled={statusCode === 'started'}
                      disabledDate={(current) => {
                        const formEndDate = form.getFieldValue('endDate');
                        const endDateFormat = moment(formEndDate).format('YYYY-MM-DD HH:mm:ss');
                        if (formEndDate && current > moment(endDateFormat, 'YYYY-MM-DD HH:mm:ss')) {
                          return true;
                        } else if (current && IsInProgramStore.isShowFeature && sprintType) { // 项目群启用 
                          const currentDateFormat = current.format('YYYY-MM-DD HH:mm:ss');
                          // 时间要在pi结束时间与开始时间内  还要满足时间不能再冲刺范围内
                          const isBan = !moment(currentDateFormat).isBefore(IsInProgramStore.getPiInfo.endDate)
                            || !moment(currentDateFormat).isAfter(IsInProgramStore.piInfo.actualStartDate || IsInProgramStore.piInfo.startDate)
                            || IsInProgramStore.stopChooseBetween(currentDateFormat, sprintId);

                          return isBan;
                        } else {
                          return false;
                        }

                        // if(current > moment(endDate, 'YYYY-MM-DD HH:mm:ss'))
                      }}
                      format="YYYY-MM-DD HH:mm:ss"
                      showTime
                    />,
                  )}
                </FormItem>

                <p style={{ marginTop: '.08rem' }}>~</p>
                <FormItem>
                  {getFieldDecorator('endDate', {
                    rules: [
                      { required: true, message: '请选择结束日期' },
                      { validator: this.checkEndDate },
                    ],


                    initialValue: endDate ? moment(endDate, 'YYYY-MM-DD HH:mm:ss') : '',
                  })(
                    <DatePicker
                      autoFocus
                      style={{ width: 165, height: 32 }}
                      allowClear
                      disabledDate={(current) => {
                        const formStartDate = form.getFieldValue('startDate');
                        const startDateFormat = moment(formStartDate).format('YYYY-MM-DD HH:mm:ss');
                        if (formStartDate && current < moment(startDateFormat, 'YYYY-MM-DD HH:mm:ss')) {
                          return true;
                        } else if (current && IsInProgramStore.isShowFeature && sprintType) { // 项目群启用
                          const currentDateFormat = current.format('YYYY-MM-DD HH:mm:ss');
                          // 时间要在pi结束时间与开始时间内  还要满足时间不能再冲刺范围内
                          const isBan = !moment(currentDateFormat).isBefore(IsInProgramStore.getPiInfo.endDate)
                            || !moment(currentDateFormat).isAfter(IsInProgramStore.piInfo.actualStartDate || IsInProgramStore.piInfo.startDate)
                            || IsInProgramStore.stopChooseBetween(currentDateFormat, sprintId);

                          return isBan;
                        } else {
                          return false;
                        }
                      }}
                      format="YYYY-MM-DD HH:mm:ss"
                      showTime

                    />,
                  )}
                </FormItem>
              </div>
            </Form>
          </Edit>
        </TextEditToggle>
      </div>
    ) : null;
  }
}

export default Form.create({})(SprintDateRange);
