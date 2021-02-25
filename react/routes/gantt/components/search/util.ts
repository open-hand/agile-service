import { toJS } from 'mobx';
import { set } from 'lodash';
import { IChosenFields } from '@/components/issue-search/store';
import { IChosenFieldField } from '@/components/chose-field/types';
import { ISearchVO } from '@/common/types';

function transformSystemFilter(data:any) {
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
    summary,
    version,
    fixVersion,
    influenceVersion,
    starBeacon,
    userId,
    testResponsibleIds,
    mainResponsibleIds,
    environment,
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
      userId,
      starBeacon,
      assigneeId,
      issueIds,
      component,
      epic,
      feature,
      label,
      summary,
      version,
      fixVersion,
      influenceVersion,
      testResponsibleIds,
      mainResponsibleIds,
      environment,
      creatorIds,
      updatorIds,
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
export function transformFilter(chosenFields: IChosenFields) {
  const customField = {
    option: [],
    date: [],
    date_hms: [],
    number: [],
    string: [],
    text: [],
  } as Required<ISearchVO['otherArgs']>['customField'];
  const systemFilter = {} as unknown as any;
  for (const [code, field] of chosenFields) {
    const { fieldType, id } = field as IChosenFieldField;
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
      case 'multiMember':
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
  set(filter.otherArgs, 'customField', customField);
  return filter;
}
