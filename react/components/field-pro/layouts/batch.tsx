import React from 'react';

import moment from 'moment';
// eslint-disable-next-line import/no-cycle
import { getAgileFields } from '../base';
import { getComponentCodeForLocalCode } from '../base/utils';
import { IFieldSystemConfig } from '../base/type';
import { AgileComponentMapProps } from '../base/component';

function getFieldConfig({
  code, fieldType, fieldOptions, id, name,
}: any): Partial<IFieldSystemConfig<AgileComponentMapProps>> {
  switch (code) {
    case 'componentIssueRelVOList':
      return { code: 'component', props: { searchable: true } };
    case 'featureId': {
      return { code: 'feature' };
    }
    case 'labelIssueRelVOList':
      return { code: 'label' };
    case 'fixVersion': {
      return { props: { statusArr: ['version_planning'] } };
    }
    case 'estimatedEndTime':
      return { props: { defaultPickerValue: moment().endOf('d') } } as any;
    case 'actualEndTime': {
      return { props: { defaultPickerValue: moment().endOf('d') } };
    }
    case 'epicId': {
      return { code: 'epic' };
    }
    case 'sprintId':
      return { code: 'sprint', props: { statusList: ['started', 'sprint_planning'] } };
    case 'statusId': {
      return { code: 'status', props: { noIssueTypeIdQuery: true } };
    }
    case 'priorityId':
      return { code: 'priority', props: {} };
    case 'pi':
      return { code: 'pi', props: { openPermission: true } };
    case 'productIds':
      return { code: 'product' };
  }
  return {};
}
/**
 * 获取批量处理字段
 * @param fields
 * @returns
 */
function getBatchFelids(fields: any[]) {
  return fields.map((field) => {
    const config = getFieldConfig(field);
    const element = getAgileFields({
      code: getComponentCodeForLocalCode(field.code),
      fieldType: field.fieldType,
      ...config,
      props: {
        name: field.code,
        label: field.name,
        style: { width: '100%' },
        fieldId: field.id,
        ...config.props,
      },
    })[0][0];
    return element as React.ReactElement;
  });
  //   getAgileFields()
}
export default getBatchFelids;
