import React from 'react';
import { observer } from 'mobx-react-lite';
import { Select } from 'choerodon-ui/pro';
import { LabelLayout } from 'choerodon-ui/pro/lib/form/enum';
import { find } from 'lodash';
import { IVersion, ISprint, IStatus } from '@/common/types';
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
  status: IStatus[]
  type: IPieChartType,
  chooseDimension: 'sprint' | 'version' | 'status' | ''
  setType: Function,
  setChooseDimension: Function,
  setChooseId: Function,
  projectId?: string
}
const PieSearch: React.FC<PieSearchProps> = ({
  chooseId, versions, sprints, status, type, chooseDimension, setType, setChooseDimension, setChooseId,
}) => {
  let chooseDimensionType = [
    {
      key: 'sprint',
      name: '冲刺',
    }, {
      key: 'version',
      name: '版本',
    }, {
      key: 'status',
      name: '状态',
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

  const changeType = (value: IPieChartType) => {
    setType(value);
    setChooseDimension('');
  };

  const handleChooseDimensionChange = (value: 'sprint' | 'version' | 'status' | '') => {
    setChooseDimension(value);
    if (value) {
      switch (value) {
        case 'sprint': {
          setChooseId(sprints[0] && sprints[0].sprintId);
          break;
        }
        case 'version': {
          setChooseId(versions[0] && versions[0].versionId);
          break;
        }
        case 'status': {
          setChooseId(status[0] && status[0].id);
          break;
        }
        default: break;
      }
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
        defaultValue={type}
        value={type}
        label="统计类型"
        onChange={changeType}
        clearButton={false}
        style={{ width: 240 }}
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
        style={{ width: 240, marginLeft: 20 }}
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
            label={find(chooseDimensionType, { key: chooseDimension })?.name}
            labelLayout={'float' as LabelLayout}
            value={chooseId?.toString()}
            onChange={handleChooseIdChange}
            clearButton
            searchable
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
            {
              chooseDimension === 'status' && status.map((s) => (
                <Option value={s.id}>
                  {s.name}
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
