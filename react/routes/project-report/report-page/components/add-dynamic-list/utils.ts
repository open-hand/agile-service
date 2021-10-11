import { toJS } from 'mobx';
import { ICustomFieldData, IExportSearch } from '@/api';
import { IChosenFieldField } from '@/components/chose-field/types';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { FieldProps } from 'choerodon-ui/pro/lib/data-set/Field';

function transformSystemFilter(data: any): Omit<IExportSearch, 'exportFieldCodes'> {
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
    fixVersion,
    influenceVersion,
    creatorIds,
    updatorIds,
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
      fixVersion,
      influenceVersion,
      creatorIds,
      updatorIds,
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
const getCustomFieldFilters = (chosenFields: Array<IChosenFieldField>, record: Record) => {
  const customField: ICustomFieldData = {
    option: [],
    date: [],
    date_hms: [],
    number: [],
    string: [],
    text: [],
  };
  const systemFilter = {} as any;
  for (let index = 0; index < chosenFields.length; index += 1) {
    const { fieldType, id, code } = chosenFields[index];
    const value = toJS(record.get(code));
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
            value: value || null,
          });
        }
        break;
      }
      case 'text': {
        if (value && value.length > 0) {
          customField.text.push({
            fieldId: id,
            value: value || null,
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

  if (record.get('sprint')) {
    systemFilter.sprint = record.get('sprint');
  }

  const filter = transformSystemFilter(systemFilter);
  filter.otherArgs.customField = customField;
  return filter;
};

function getFilterFormSystemFields(isInProgram: boolean): FieldProps[] {
  return ([{
    name: 'statusId',
    label: '状态',
    valueField: 'id',
    textField: 'name',
  }, {
    name: 'sprint',
    label: '冲刺',
    required: true,
    valueField: 'sprintId',
    textField: 'sprintName',
  }, {
    name: 'issueTypeId',
    label: '工作项类型',
    valueField: 'id',
    textField: 'name',
  },
  ...isInProgram ? [{
    name: 'feature',
    label: '所属特性',
    valueField: 'issueId',
    textField: 'summary',
  }] : [{
    name: 'epic',
    label: '史诗',
    valueField: 'issueId',
    textField: 'epicName',
  }],
  {
    name: 'feature',
    label: '特性',
    valueField: 'issueId',
    textField: 'summary',
  },
  {
    name: 'epic',
    label: '所属史诗',
    valueField: 'issueId',
    textField: 'epicName',
  },
  {
    name: 'priorityId',
    label: '优先级',
    valueField: 'id',
    textField: 'name',
  }, {
    name: 'label',
    label: '标签',
    valueField: 'labelId',
    textField: 'labelName',
  }, {
    name: 'component',
    label: '模块',
    valueField: 'componentId',
    textField: 'name',
  }, {
    name: 'version',
    label: '版本',
    valueField: 'versionId',
    textField: 'name',
  }]);
}
export { getCustomFieldFilters, getFilterFormSystemFields };
