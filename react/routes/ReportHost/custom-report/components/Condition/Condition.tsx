import React, {
  ReactElement, useCallback, useEffect, useState,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  Select, Form, DataSet, TextArea,
} from 'choerodon-ui/pro';
import { Icon, Tooltip } from 'choerodon-ui';
import { IField } from '@/common/types';
import { pageConfigApi } from '@/api';
import styles from './Condition.less';

const { Option } = Select;

const Part: React.FC<{
  title: string
  tip?: string
  children: string | ReactElement | ReactElement[] | null | Array<ReactElement | null>
}> = ({ title, children, tip }) => (
  <div className={styles.part}>
    <div className={styles.title}>
      <span className={styles.text}>{title}</span>
      {!!tip && <Tooltip title={tip}><Icon type="info_outline" className={styles.tip} /></Tooltip>}
    </div>
    <div className={styles.content}>{children}</div>
  </div>
);
const Condition: React.FC<{
  addReportDs: DataSet,
}> = ({ addReportDs }) => {
  const [dimension, setDimension] = useState<IField[]>([]);
  useEffect(() => {
    pageConfigApi.load().then((res: { content: IField[]}) => {
      setDimension(res.content.filter((item) => ['single', 'checkbox', 'multiple', 'radio', 'member', 'multiMember'].includes(item.fieldType)));
    });
  }, []);
  return (
    <div className={styles.condition}>
      <Part title="图表说明">
        <Form dataSet={addReportDs} className={styles.form}>
          <TextArea name="description" autoSize />
        </Form>
      </Part>
      <Part title="数据设置" tip="您可以在此设置图表的分析维度、对比维度，以及统计单位。例如：分析维度为优先级，对比维度为迭代，统计单位为问题计数。那么图表展示的就是各个优先级在各个迭代的问题数量。">
        <Form dataSet={addReportDs} className={styles.form}>
          <Select name="type" clearButton={false}>
            <Option value="line">基础折线图</Option>
            <Option value="pie">饼图</Option>
            <Option value="bar">柱状图</Option>
            <Option value="stackedBar">堆叠柱状图</Option>
          </Select>
          <Select name="row" clearButton={false}>
            {
              dimension.filter((item) => item.id !== addReportDs.current?.get('column')).map((item) => (
                <Option value={item.id}>{item.name}</Option>
              ))
            }
          </Select>
          {
          addReportDs.current?.get('type') === 'stackedBar' && (
            <Select name="column" clearButton={false}>
              {
              dimension.filter((item) => item.id !== addReportDs.current?.get('row')).map((item) => (
                <Option value={item.id}>{item.name}</Option>
              ))
            }
            </Select>
          )
        }
          <Select name="unit" clearButton={false}>
            <Option value="storyPoint">故事点</Option>
            <Option value="issueCount">问题计数</Option>
          </Select>
        </Form>
      </Part>
      <Part title="数据筛选" tip="设置图表的数据范围。例如：您可以选择一部分迭代的问题进行分析">描述</Part>
    </div>
  );
};

export default observer(Condition);
