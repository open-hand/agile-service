import React from 'react';

import moment from 'moment';
// eslint-disable-next-line import/no-cycle
import { getAgileFields } from '../base';

function getFieldConfig({
  code, fieldType, fieldOptions, id, name,
}: any) {
  switch (code) {
    case 'componentIssueRelVOList':
      return { code: 'component', props: { searchable: true } };
    case 'featureId': {
      return { code: 'feature' };
    }
    case 'estimatedEndTime':
      return { props: { defaultPickerValue: moment().endOf('d') } };
    case 'tags': {
      return { code: 'tag' };
    }
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
      code: field.code,
      fieldType: field.fieldType,
      ...config,
      props: {
        name: field.code,
        label: field.name,
        style: { width: '100%' },
        ...config.props,
      },
    })[0][0];
    return element as React.ReactElement;
  });
  //   getAgileFields()
}
export default getBatchFelids;
