import React from 'react';
import { observer } from 'mobx-react-lite';
import { Select } from 'choerodon-ui/pro';
import { LabelLayout } from 'choerodon-ui/pro/lib/form/enum';
import { IChartSearchAdditionalProps } from '../types';

const { Option } = Select;
export type IUnit = 'story_point' | 'issue_count' | 'remain_time';

export interface IterationSpeedSearchProps extends IChartSearchAdditionalProps {
  unit: IUnit,
  setUnit: Function,
  projectId: string,
}

const IterationSpeedSearch: React.FC<IterationSpeedSearchProps> = ({ unit, setUnit, searchDataSet }) => {
  const handleChangeCurrentUnit = (value: IUnit) => {
    setUnit(value);
  };

  return (
    <div>
      <Select
        labelLayout={'float' as LabelLayout}
        clearButton={false}
        style={{ width: 500, marginBottom: 20 }}
        label="单位选择"
        value={unit}
        dataSet={searchDataSet}
        onChange={handleChangeCurrentUnit}
      >
        <Option key="story_point" value="story_point">
          故事点
        </Option>
        <Option key="issue_count" value="issue_count">
          工作项计数
        </Option>
        <Option key="remain_time" value="remain_time">
          剩余时间
        </Option>
      </Select>
    </div>
  );
};

export default observer(IterationSpeedSearch);
