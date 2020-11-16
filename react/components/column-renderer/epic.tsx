import React from 'react';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import EpicTag from './base/EpicTag';

const renderEpic = ({ record }: { record: Record }) => (
  <EpicTag
    color={record.get('epicColor')}
    name={record.get('epicName')}
  />
);
export default renderEpic;
