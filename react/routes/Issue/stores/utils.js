import { castArray } from 'lodash';
import { toJS } from 'mobx';
import { getDateValue } from '@/components/issue-search/utils';

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
    actualStartTime,
    actualEndTime,
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
    myAssigned,
    userId,
    testResponsibleIds,
    mainResponsibleIds,
    creatorIds,
    updatorIds,
    environment,
    appVersion,
    tags,
    storyPointsNull,
    remainingTimeNull,
    storyPoints,
    remainingTime,
  } = data;
  return {
    advancedSearchArgs: {
      issueTypeId,
      reporterIds,
      statusId,
      priorityId,
      storyPointsNull,
      remainingTimeNull,
      storyPoints,
      remainingTime,
    },
    otherArgs: {
      userId,
      starBeacon,
      myAssigned,
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
      estimatedStartTimeScopeStart: getDateValue(estimatedStartTime, 0),
      estimatedStartTimeScopeEnd: getDateValue(estimatedStartTime, 1),
      estimatedEndTimeScopeStart: getDateValue(estimatedEndTime, 0),
      estimatedEndTimeScopeEnd: getDateValue(estimatedEndTime, 1),
      actualStartTimeScopeStart: getDateValue(actualStartTime, 0),
      actualStartTimeScopeEnd: getDateValue(actualStartTime, 1),
      actualEndTimeScopeStart: getDateValue(actualEndTime, 0),
      actualEndTimeScopeEnd: getDateValue(actualEndTime, 1),
      createStartDate: getDateValue(createDate, 0),
      createEndDate: getDateValue(createDate, 1),
      updateStartDate: getDateValue(updateDate, 0),
      updateEndDate: getDateValue(updateDate, 1),
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
          value: value ? castArray(value) : value,
        });
        break;
      }
      case 'input': {
        customField.string.push({
          fieldId: id,
          value: value || null,
        });
        break;
      }
      case 'text': {
        customField.text.push({
          fieldId: id,
          value: value || null,
        });
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
