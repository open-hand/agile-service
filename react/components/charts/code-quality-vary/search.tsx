import React from 'react';
import { Select } from 'choerodon-ui/pro';
import { LabelLayout } from 'choerodon-ui/pro/lib/form/enum';

const { Option } = Select;
export interface CodeQualityVarySearchProps {
  projectId: string,
  days: number
  setDays: (days: number) => void
}
const CodeQualityVarySearch: React.FC<CodeQualityVarySearchProps> = ({
  days,
  setDays,
}) => (
  <div>
    <Select value={days} onChange={setDays} labelLayout={'float' as LabelLayout}>
      <Option value={7}>
        近7天
      </Option>
      <Option value={15}>
        近15天
      </Option>
      <Option value={30}>
        近30天
      </Option>
    </Select>
  </div>
);
export default CodeQualityVarySearch;
