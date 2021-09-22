import React from 'react';
import { observer } from 'mobx-react-lite';
import { Icon } from 'choerodon-ui/pro';
import { Select } from 'choerodon-ui/pro';
import { find } from 'lodash';
import to from '@/utils/to';
import LINK_URL from '@/constants/LINK_URL';
import { LabelLayout } from 'choerodon-ui/pro/lib/form/enum';

const { Option } = Select;

export interface IVersion {
  versionId: string,
  name: string,
  statusCode: 'released' | 'version_planning' | 'archived',
  releaseDate: string,
}

export type IUnit = 'story_point' | 'issue_count' | 'remain_time';

export interface VersionReportSearchProps {
  unit: IUnit
  setUnit: Function
  versions: IVersion[]
  versionId: string
  setVersionId: Function
  projectId?: string
}

const VersionReportSearch:React.FC<VersionReportSearchProps> = ({
  unit, setUnit, versions, versionId, setVersionId,
}) => {
  const currentVersion = find(versions, { versionId });

  const handleChangeVersion = (value: string) => {
    setVersionId(value);
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
          label="版本"
          value={versionId}
          onChange={handleChangeVersion}
        >
          {
          versions.map((version: IVersion) => (
            <Option
              key={version.versionId}
              value={version.versionId}
            >
              {version.name}
            </Option>
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
          <Option key="issue_count" value="issue_count">问题计数</Option>
          <Option key="remain_time" value="remain_time">剩余时间</Option>
        </Select>
      </div>
      <div style={{ marginTop: 10, display: 'flex', justifyContent: 'space-between' }}>
        <p style={{ marginBottom: 0 }}>{versionId && currentVersion && currentVersion.statusCode === 'released' ? `发布于 ${currentVersion.releaseDate ? currentVersion.releaseDate.split(' ')[0] : '未指定发布日期'}` : '未发布'}</p>
        {versions?.length ? (
          <p
            className="primary"
            style={{
              cursor: 'pointer',
              display: 'flex',
              alignItems: 'center',
              marginBottom: 0,
              color: '#5365EA',
              whiteSpace: 'nowrap',
            }}
            role="none"
            onClick={() => {
              to(LINK_URL.workListIssue, {
                type: 'project',
                params: {
                  paramType: 'version',
                  paramId: versionId,
                  paramName: `${currentVersion && currentVersion.name}下的问题`,
                },
              }, { blank: true });
            }}
          >
            在“所有问题中”查看
            <Icon style={{ fontSize: 13 }} type="open_in_new" />
          </p>
        ) : null}
      </div>
    </div>
  );
};

export default observer(VersionReportSearch);
