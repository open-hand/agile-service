import React, { Component, createRef } from 'react';
import moment from 'moment';
import { observer } from 'mobx-react';
import { DatePicker } from 'choerodon-ui';
import TextEditToggle from '@/components/TextEditToggle';
import { getProjectId } from '@/common/utils';
import BacklogStore from '@/stores/project/backlog/BacklogStore';

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
    BacklogStore.axiosUpdateSprint(req).then((res) => {
      BacklogStore.updateSprint(sprintId, {
        objectVersionNumber: res.objectVersionNumber,
        startDate: res.startDate,
        endDate: res.endDate,
      });     
    }).catch((error) => {
    });
  };

  render() {
    const {
      data: { statusCode, startDate, endDate }, disabled,
    } = this.props;
    return statusCode === 'started' ? (
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
                disabledDate={endDate ? current => current > moment(endDate, 'YYYY-MM-DD HH:mm:ss') : ''}
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
                disabledDate={startDate ? current => current < moment(startDate, 'YYYY-MM-DD HH:mm:ss') : ''}         
                format="YYYY-MM-DD HH:mm:ss"
                showTime
                onChange={(date, dateString) => {
                  this.handleUpdateDate('endDate', dateString);           
                }}
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
