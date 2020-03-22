import React, { Component, createRef } from 'react';
import moment from 'moment';
import { observer } from 'mobx-react';
import { DatePicker, message } from 'choerodon-ui';
import TextEditToggle from '@/components/TextEditToggle';
import { getProjectId, catchFailed } from '@/common/utils';
import BacklogStore from '@/stores/project/backlog/BacklogStore';
import IsInProgramStore from '../../../../../stores/common/program/IsInProgramStore';

const { Text, Edit } = TextEditToggle;

@observer class SprintDateRange extends Component {
  constructor() {
    super();
    this.startDateEdit = createRef();
    this.endDateEdit = createRef();
  }

  handleUpdateDate = (type, dateString) => {
    const date = `${dateString} 00:00:00`;
    const { data } = this.props;
    const { objectVersionNumber, sprintId } = data;
    const req = {
      objectVersionNumber,
      projectId: getProjectId(),
      sprintId: data.sprintId,
      [type]: date,
    };
    BacklogStore.axiosUpdateSprint(req, IsInProgramStore.isShowFeature).then(res => catchFailed(res)).then((res) => {
      BacklogStore.updateSprint(sprintId, {
        objectVersionNumber: res.objectVersionNumber,
        startDate: res.startDate,
        endDate: res.endDate,
      });
    }).catch((error) => {
      message.error(error);
    });
  };

  // console.log('data', data);
  render() {
    const {
      data: {
        statusCode, startDate, endDate, sprintId,
      }, disabled,
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
          disabled={disabled || statusCode === 'started'}
          saveRef={this.startDateEdit}
        >
          <Text>
            <div
              className="c7n-backlog-sprintDataItem"
              role="none"
            >
              {startDate ? moment(startDate, 'YYYY-MM-DD HH:mm:ss').format('YYYY年MM月DD日 HH:mm:ss') : '无'}
            </div>
          </Text>
          <Edit>
            {({ width }) => (
              <DatePicker
                autoFocus
                style={{ width, height: 32 }}
                allowClear={false}
                defaultOpen
                defaultValue={startDate ? moment(startDate, 'YYYY-MM-DD HH-mm-ss') : ''}
                disabledDate={endDate ? (current) => {
                  // return true;
                  const endDateFormat = moment(endDate).format('YYYY-MM-DD HH:mm:ss');
                  if (current > moment(endDateFormat, 'YYYY-MM-DD HH:mm:ss')) {
                    return true;
                  } else if (IsInProgramStore.isShowFeature) { // 项目群启用 
                    const currentDateFormat = current.format('YYYY-MM-DD HH:mm:ss');
                    // 时间要在pi结束时间与开始时间内  还要满足时间不能再冲刺范围内
                    let isBan = !moment(currentDateFormat).isBefore(IsInProgramStore.getPiInfo.endDate)
                      || !moment(currentDateFormat).isAfter(IsInProgramStore.piInfo.actualStartDate || IsInProgramStore.piInfo.startDate)
                      || IsInProgramStore.stopChooseBetween(currentDateFormat, sprintId);
                    // eslint-disable-next-line no-plusplus
                    if (!isBan) {
                      const minTime = IsInProgramStore.findDateMinRange(endDateFormat, sprintId);
                      if (moment(currentDateFormat).isBefore(minTime)) {
                        isBan = true;
                      }
                    }

                    return isBan;
                  } else {
                    return false;
                  }

                  // if(current > moment(endDate, 'YYYY-MM-DD HH:mm:ss'))
                } : ''}
                format="YYYY-MM-DD HH:mm:ss"
                showTime
                onChange={(date, dateString) => {
                  this.handleUpdateDate('startDate', dateString);
                }}
                onOpenChange={(open) => {
                  if (!open) {
                    this.startDateEdit.current.leaveEditing();
                  }
                }}
              />
            )}

          </Edit>
        </TextEditToggle>
        <p>~</p>
        <TextEditToggle
          disabled={disabled}
          saveRef={this.endDateEdit}
        >
          <Text>
            <div
              className="c7n-backlog-sprintDataItem"
            >
              {endDate ? moment(endDate, 'YYYY-MM-DD HH:mm:ss').format('YYYY年MM月DD日 HH:mm:ss') : '无'}
            </div>
          </Text>
          <Edit>
            {({ width }) => (
              <DatePicker
                autoFocus
                style={{ width, height: 32 }}
                allowClear={false}
                defaultOpen
                defaultValue={endDate ? moment(endDate, 'YYYY-MM-DD HH-mm-ss') : ''}
                disabledDate={startDate ? (current) => {
                  const startDateFormat = moment(startDate).format('YYYY-MM-DD HH:mm:ss');

                  if (current < moment(startDate, 'YYYY-MM-DD HH:mm:ss')) {
                    return true;
                  } else if (IsInProgramStore.isShowFeature) { // 项目群启用
                    const currentDateFormat = current.format('YYYY-MM-DD HH:mm:ss');
                    // 时间要在pi结束时间与开始时间内  还要满足时间不能再冲刺范围内
                    let isBan = !moment(currentDateFormat).isBefore(IsInProgramStore.getPiInfo.endDate)
                      || !moment(currentDateFormat).isAfter(IsInProgramStore.piInfo.actualStartDate || IsInProgramStore.piInfo.startDate)
                      || IsInProgramStore.stopChooseBetween(currentDateFormat, sprintId);
                    // eslint-disable-next-line no-plusplus
                    if (!isBan) {
                      const maxTime = IsInProgramStore.findDateMaxRange(startDateFormat, sprintId);
                      if (moment(currentDateFormat).isAfter(maxTime)) {
                        isBan = true;
                      }
                    }
                    return isBan;
                  } else {
                    return false;
                  }
                } : ''}
                format="YYYY-MM-DD HH:mm:ss"
                showTime
                onChange={(date, dateString) => {
                  this.handleUpdateDate('endDate', dateString);
                }}
                d
                onOpenChange={(open) => {
                  if (!open) {
                    this.endDateEdit.current.leaveEditing();
                  }
                }}
              />
            )}
          </Edit>
        </TextEditToggle>
      </div>
    ) : null;
  }
}

export default SprintDateRange;
