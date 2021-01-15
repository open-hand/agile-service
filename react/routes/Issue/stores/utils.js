import React from 'react';
import { toJS } from 'mobx';
import { observer } from 'mobx-react-lite';
import { Button } from 'choerodon-ui';
import openBatchDeleteModal from '@/components/BatchDeleteConfirm';
import Modal from '../components/Modal';
import BatchModal from '../components/BatchModal';

function transformSystemFilter(data) {
  const {
    issueTypeId,
    assigneeId,
    statusId,
    priorityId,
    issueIds,
    quickFilterIds,
    createDate = [],
    updateDate = [],
    estimatedStartTime = [],
    estimatedEndTime = [],
    contents,
    component,
    epic,
    feature,
    label,
    reporterIds,
    sprint,
    summary,
    version,
    fixVersion,
    influenceVersion,
    starBeacon,
    userId,
    testResponsibleIds,
    mainResponsibleIds,
    environment,
  } = data;
  return {
    advancedSearchArgs: {
      issueTypeId,
      reporterIds,
      statusId,
      priorityId,
    },
    otherArgs: {
      userId,
      starBeacon,
      assigneeId,
      issueIds,
      component,
      epic,
      feature,
      label,
      sprint,
      summary,
      version,
      fixVersion,
      influenceVersion,
      testResponsibleIds,
      mainResponsibleIds,
      environment,
    },
    searchArgs: {
      estimatedStartTimeScopeStart: estimatedStartTime[0],
      estimatedStartTimeScopeEnd: estimatedStartTime[1],
      estimatedEndTimeScopeStart: estimatedEndTime[0],
      estimatedEndTimeScopeEnd: estimatedEndTime[1],
      createStartDate: createDate[0],
      createEndDate: createDate[1],
      updateStartDate: updateDate[0],
      updateEndDate: updateDate[1],
    },
    quickFilterIds,
    contents,
  };
}
export function transformFilter(chosenFields) {
  const customField = {
    option: [],
    date: [],
    date_hms: [],
    number: [],
    string: [],
    text: [],
  };
  const systemFilter = {};
  for (const [code, field] of chosenFields) {
    const { fieldType, id } = field;
    const value = toJS(field.value);
    if (value === undefined || value === null || value === '') {
      // eslint-disable-next-line no-continue
      continue;
    }
    // 系统字段
    if (!id) {
      systemFilter[code] = value;
      // eslint-disable-next-line no-continue
      continue;
    }
    switch (fieldType) {
      case 'single':
      case 'multiple':
      case 'radio':
      case 'checkbox':
      case 'member': {
        const v = Array.isArray(value) ? value : [value];
        if (v.length > 0) {
          customField.option.push({
            fieldId: id,
            value: v,
          });
        }
        break;
      }
      case 'input': {
        if (value && value.length > 0) {
          customField.string.push({
            fieldId: id,
            value,
          });
        }
        break;
      }
      case 'text': {
        if (value && value.length > 0) {
          customField.text.push({
            fieldId: id,
            value,
          });
        }
        break;
      }
      case 'number': {
        customField.number.push({
          fieldId: id,
          value,
        });
        break;
      }
      case 'time':
      case 'datetime':
      case 'date': {
        if (value && value.length > 0) {
          if (fieldType === 'time') {
            customField.date_hms.push({
              fieldId: id,
              startDate: value[0],
              endDate: value[1],
            });
          } else {
            customField.date.push({
              fieldId: id,
              startDate: value[0],
              endDate: value[1],
            });
          }
        }
        break;
      }
      default: break;
    }
  }
  const filter = transformSystemFilter(systemFilter);
  filter.otherArgs.customField = customField;
  return filter;
}

let modal;
function Header({
  dataSet, close, onClickEdit, onClickDelete, hasBatchDeletePermission,
}) {
  return (
    <>
      <div style={{ display: 'flex', alignItems: 'center' }}>
        <span style={{
          fontSize: '30px', fontWeight: 500, marginRight: 12, color: '#FFFFFF',
        }}
        >
          {`${dataSet.selected.length}`}
        </span>
        <span style={{ fontSize: '16px', color: 'rgba(255, 255, 255, 0.8)', marginTop: 5 }}>项已选中</span>
      </div>
      <div style={{
        marginLeft: 'auto', height: 56, display: 'flex', alignItems: 'center',
      }}
      >
        {
          hasBatchDeletePermission && (
            <div style={{
              display: 'inline-block', height: 56, lineHeight: '56px', borderRight: '1px solid #95A5FF',
            }}
            >
              <Button
                icon="mode_edit"
                style={{ color: 'white', marginRight: 6 }}
                onClick={onClickEdit}
              >
                编辑
              </Button>
              <Button
                icon="delete_forever"
                style={{ color: 'white', marginRight: 18 }}
                onClick={onClickDelete}
              >
                删除
              </Button>
            </div>
          )
        }
        <Button
          icon="close"
          shape="circle"
          style={{ color: 'white', marginRight: -10, marginLeft: 10 }}
          onClick={close}
        />
      </div>
    </>
  );
}
const ObserverHeader = observer(Header);
export function handleSelect({ dataSet }, issueSearchStore, hasBatchDeletePermission) {
  const close = () => {
    dataSet.unSelectAll();
    issueSearchStore.setBatchAction(undefined);
  };
  if (!hasBatchDeletePermission) {
    issueSearchStore.setBatchAction('edit');
  }
  modal = Modal.open({
    key: 'modal',
    header: <ObserverHeader
      dataSet={dataSet}
      modal={modal}
      close={() => {
        modal.close();
        close();
      }}
      onClickEdit={() => {
        issueSearchStore.setBatchAction('edit');
      }}
      onClickDelete={() => {
        modal.close();
        issueSearchStore.setBatchAction('delete');
        openBatchDeleteModal({ dataSet, close });
      }}
      hasBatchDeletePermission={hasBatchDeletePermission}
    />,
    content: <BatchModal
      dataSet={dataSet}
      modal={modal}
      fields={issueSearchStore.fields}
      onCancel={() => {
        modal.close();
        close();
      }}
      onEdit={() => {
        modal.close();
        close();
        dataSet.query();
      }}
      issueSearchStore={issueSearchStore}
    />,
  });
}
export function handleUnSelect({ dataSet }) {
  if (dataSet.selected.length === 0 && modal) {
    modal.close();
  }
}
