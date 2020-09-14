import React from 'react';
import { toJS } from 'mobx';
import { observer } from 'mobx-react-lite';
import { Button } from 'choerodon-ui';
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
    contents,
    component,
    epic,
    feature,
    label,
    reporterIds,
    sprint,
    summary,
    version,
  } = data;
  return {
    advancedSearchArgs: {
      issueTypeId,
      reporterIds,
      statusId,
      priorityId,
    },
    otherArgs: {
      assigneeId,
      issueIds,
      component,
      epic,
      feature,
      label,
      sprint,
      summary,
      version,
    },
    searchArgs: {
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
function Header({ dataSet, close }) {
  return (
    <>
      <div style={{ fontSize: '18px', fontWeight: 500, marginRight: 12 }}>
        {`批量编辑 (已选中${dataSet.selected.length}项)`}
      </div>
      <Button
        icon="close"
        shape="circle"
        style={{ color: 'white', marginRight: -5, marginLeft: 'auto' }}
        onClick={close}
      />
    </>
  );
}
const ObserverHeader = observer(Header);
export function handleSelect({ dataSet }, issueSearchStore) {
  modal = Modal.open({
    key: 'modal',
    header: <ObserverHeader
      dataSet={dataSet}
      modal={modal}
      close={() => {
        modal.close();
        dataSet.unSelectAll();
      }}
    />,
    content: <BatchModal
      dataSet={dataSet}
      modal={modal}
      fields={issueSearchStore.fields}
      onCancel={() => {
        modal.close();
        dataSet.unSelectAll();
      }}
      onEdit={() => {
        modal.close();
        dataSet.unSelectAll();
        dataSet.query();
      }}
    />,
  });
}
export function handleUnSelect({ dataSet }) {
  if (dataSet.selected.length === 0 && modal) {
    modal.close();
  }
}
