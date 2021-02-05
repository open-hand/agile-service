import React from 'react';
import { observer } from 'mobx-react-lite';
import { Select } from 'choerodon-ui/pro';
import { IVersion, ISprint } from '@/common/types';
import { LabelLayout } from 'choerodon-ui/pro/lib/form/enum';
import styles from './index.less';
import { IPieChartType } from './index';

const { Option } = Select;

export const types = [
  { title: '经办人', value: 'assignee' },
  { title: '模块', value: 'component' },
  { title: '问题类型', value: 'typeCode' },
  { title: '版本', value: 'version' },
  { title: '优先级', value: 'priority' },
  { title: '状态', value: 'status' },
  { title: '冲刺', value: 'sprint' },
  { title: '史诗', value: 'epic' },
  { title: '标签', value: 'label' },
];

export interface PieSearchProps {
  chooseId: string | '',
  versions: IVersion[],
  sprints: ISprint[],
  type: IPieChartType,
  chooseDimension: 'sprint' | 'version' | ''
  setType: Function,
  setChooseDimension: Function,
  setChooseId: Function,
  projectId?: string
}
const PieSearch: React.FC<PieSearchProps> = ({
  chooseId, versions, sprints, type, chooseDimension, setType, setChooseDimension, setChooseId,
}) => {
  let chooseDimensionType = [
    {
      key: 'sprint',
      name: '冲刺',
    }, {
      key: 'version',
      name: '版本',
    },
  ];

  if (type === 'sprint') {
    chooseDimensionType = [
      {
        key: 'version',
        name: '版本',
      },
    ];
  }

  if (type === 'version') {
    chooseDimensionType = [
      {
        key: 'sprint',
        name: '冲刺',
      },
    ];
  }

  const chooseDimensionTypeItem = chooseDimensionType.find((item) => item.key === chooseDimension);

  const changeType = (value: IPieChartType) => {
    setType(value);
    setChooseDimension('');
  };

  const handleChooseDimensionChange = (value: 'sprint' | 'version' | '') => {
    setChooseDimension(value);
    if (value) {
      setChooseId(value === 'version' ? versions[0] && versions[0].versionId : sprints[0] && sprints[0].sprintId);
    }
  };

  const handleChooseIdChange = (value: string) => {
    setChooseId(value);
  };

  return (
    <div className={styles.c7n_pieChart_filter}>
      <Select
        className={styles.c7n_pieChart_filter_item}
        labelLayout={'float' as LabelLayout}
        // @ts-ignore
        getPopupContainer={(triggerNode) => triggerNode.parentNode}
        defaultValue={type}
        value={type}
        label="统计类型"
        onChange={changeType}
        clearButton={false}
      >
        {
          types.map((item) => (
            <Option value={item.value} key={item.title}>{item.title}</Option>
          ))
        }
      </Select>
      <Select
        className={styles.c7n_pieChart_filter_item}
        labelLayout={'float' as LabelLayout}
        style={{ minWidth: 70 }}
        label="选择维度"
        defaultValue={chooseDimensionType[0].name}
        value={chooseDimension}
        onChange={handleChooseDimensionChange}
        clearButton
      >
        {
          chooseDimensionType.map((item) => (
            <Option
              key={item.key}
              value={item.key}
            >
              {item.name}
            </Option>
          ))
        }
      </Select>
      {
        chooseDimension ? (
          <Select
            key={chooseDimension}
            className={styles.c7n_pieChart_filter_item}
            style={{ minWidth: 200 }}
            labelLayout={'float' as LabelLayout}
            value={chooseId?.toString()}
            onChange={handleChooseIdChange}
            clearButton
          >
            {
            chooseDimension === 'version' && versions.map((item) => (
              <Option key={item.versionId} value={item.versionId}>{item.name}</Option>
            ))
          }
            {
            chooseDimension === 'sprint' && sprints.map((item) => (
              <Option key={item.sprintId} value={item.sprintId}>
                {item.sprintName}
              </Option>
            ))
          }
          </Select>
        ) : ''
      }
    </div>
  );
};

export default observer(PieSearch);
