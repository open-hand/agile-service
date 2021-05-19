import { toArray } from 'lodash';
import { toJS } from 'mobx';

function transformSystemFilter(data) {
  const {
    issueTypeId,
    assigneeId,
    statusId,
    priorityId,
    issueIds,
    quickFilterIds,
    createDate,
    updateDate,
    estimatedStartTime,
    estimatedEndTime,
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
    creatorIds,
    updatorIds,
    environment,
    appVersion,
    tags,
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
      creatorIds,
      updatorIds,
      appVersion,
      tags,
    },
    searchArgs: {
      estimatedStartTimeScopeStart: estimatedStartTime ? estimatedStartTime[0] ?? null : undefined,
      estimatedStartTimeScopeEnd: estimatedStartTime ? estimatedStartTime[1] ?? null : undefined,
      estimatedEndTimeScopeStart: estimatedEndTime ? estimatedEndTime[0] ?? null : undefined,
      estimatedEndTimeScopeEnd: estimatedEndTime ? estimatedEndTime[1] ?? null : undefined,
      createStartDate: createDate ? createDate[0] ?? null : undefined,
      createEndDate: createDate ? createDate[1] ?? null : undefined,
      updateStartDate: updateDate ? updateDate[0] ?? null : undefined,
      updateEndDate: updateDate ? updateDate[1] ?? null : undefined,
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
    if (value === undefined) {
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
        customField.option.push({
          fieldId: id,
          value: value ? toArray(value) : value,
        });
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
        if (fieldType === 'time') {
          customField.date_hms.push({
            fieldId: id,
            startDate: value ? value[0] : value,
            endDate: value ? value[1] : value,
          });
        } else {
          customField.date.push({
            fieldId: id,
            startDate: value ? value[0] : value,
            endDate: value ? value[1] : value,
          });
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
