import React, { Component, createRef } from 'react';
import moment from 'moment';
import { observer } from 'mobx-react';
import { DatePicker, message, Form } from 'choerodon-ui';
import TextEditToggle from '@/components/TextEditToggle';
import { getProjectId, catchFailed } from '@/utils/common';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import IsInProgramStore from '@/stores/common/program/IsInProgramStore';

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
        BacklogStore.loadPiInfoAndSprint();
      }
    }).catch((error) => {
      message.error(error);
    });
  };

  render() {
    const {
      data: {
        statusCode, startDate, endDate, sprintId, sprintType, planning, piId,
      }, form: { getFieldDecorator }, form, noPermission,
    } = this.props;
    return (
      <div
        className="c7n-backlog-sprintData"
        style={{
          display: 'flex',
          alignItems: 'center',
        }}
        role="none"
      >
        <TextEditToggle
          disabled={sprintType === 'ip' || noPermission} // ip冲刺禁止修改时间
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
                    ],
                    initialValue: startDate ? moment(startDate, 'YYYY-MM-DD HH:mm:ss') : '',
                  })(
                    <DatePicker
                      autoFocus
                      style={{ width: 165, height: 32 }}
                      allowClear
                      disabledDate={(date) => {
                        if (!date) {
                          return false;
                        }
                        // eslint-disable-next-line no-shadow
                        const endDate = form.getFieldValue('endDate');
                        if (!sprintType) {
                          if (endDate) {
                            return date >= endDate;
                          } else {
                            return false;
                          }
                        } else {
                          const pi = planning === true ? BacklogStore.piMap.get(piId).pi : undefined;
                          const sprints = planning === true ? BacklogStore.piMap.get(piId).sprints : undefined;
                          // 没选结束时间的时候，只判断时间点能不能选
                          // eslint-disable-next-line no-lonely-if
                          if (!endDate) {
                            return !BacklogStore.dateCanChoose({
                              date, sprintId, pi, sprints, 
                            });
                          } else {
                            // 选了结束时间之后，判断形成的时间段是否和其他重叠
                            return !BacklogStore.rangeCanChoose({
                              startDate: date, endDate, sprintId, pi, sprints,  
                            });
                          }                  
                        }
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
                    ],
                    initialValue: endDate ? moment(endDate, 'YYYY-MM-DD HH:mm:ss') : '',
                  })(
                    <DatePicker
                      autoFocus
                      style={{ width: 165, height: 32 }}
                      allowClear
                      disabledDate={(date) => {
                        if (!date) {
                          return false;
                        }
                        // eslint-disable-next-line no-shadow
                        const startDate = form.getFieldValue('startDate');
                        if (!sprintType) {
                          if (startDate) {
                            return date <= startDate;
                          } else {
                            return false;
                          }
                        } else {
                          const pi = planning === true ? BacklogStore.piMap.get(piId).pi : undefined;
                          const sprints = planning === true ? BacklogStore.piMap.get(piId).sprints : undefined;
                          // 没选开始时间的时候，只判断时间点能不能选
                          // eslint-disable-next-line no-lonely-if
                          if (!startDate) {
                            return !BacklogStore.dateCanChoose({
                              date, sprintId, pi, sprints,  
                            });
                          } else {
                            // 选了开始时间之后，判断形成的时间段是否和其他重叠
                            return !BacklogStore.rangeCanChoose({
                              startDate, endDate: date, sprintId, pi, sprints,  
                            });
                          }
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
    );
  }
}

export default Form.create({})(SprintDateRange);
