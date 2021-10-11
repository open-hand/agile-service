import React from 'react';
import { observer } from 'mobx-react-lite';
import { Icon } from 'choerodon-ui/pro';
import { Select } from 'choerodon-ui/pro';
import { find } from 'lodash';
import to from '@/utils/to';
import LINK_URL from '@/constants/LINK_URL';
import { LabelLayout } from 'choerodon-ui/pro/lib/form/enum';

const { Option } = Select;

export interface IEpic {
  issueId: string
  epicName: string
  summary?: string
}

export type IUnit = 'story_point' | 'issue_count' | 'remain_time';

export interface EpicReportSearchProps {
  unit: IUnit
  setUnit: Function
  epics: IEpic[]
  epicId: string
  setEpicId: Function
  projectId?: string
}

const EpicReportSearch:React.FC<EpicReportSearchProps> = ({
  unit, setUnit, epics, epicId, setEpicId,
}) => {
  const handleChangeEpic = (value: string) => {
    setEpicId(value);
  };

  const handleChangeUnit = (value: IUnit) => {
    setUnit(value);
  };

  return (
    <div style={{ marginBottom: 20 }}>
      <div style={{ display: 'flex' }}>
        <Select
          labelLayout={'float' as LabelLayout}
          clearButton={false}
          style={{ width: 240 }}
          label="史诗"
          value={epicId}
          onChange={handleChangeEpic}
        >
          {
          epics.map((epic: IEpic) => (
            <Option key={epic.issueId} value={epic.issueId}>{epic.epicName}</Option>
          ))
        }
        </Select>
        <Select
          labelLayout={'float' as LabelLayout}
          clearButton={false}
          style={{ width: 240, marginLeft: 20 }}
          label="单位"
          value={unit}
          onChange={handleChangeUnit}
        >
          <Option key="story_point" value="story_point">故事点</Option>
          <Option key="issue_count" value="issue_count">工作项计数</Option>
          <Option key="remain_time" value="remain_time">剩余时间</Option>
        </Select>
      </div>
    </div>
  );
};

export default observer(EpicReportSearch);
