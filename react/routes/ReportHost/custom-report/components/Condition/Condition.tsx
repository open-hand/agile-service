import React, {
  ReactElement,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  Select, Form, DataSet, TextArea,
} from 'choerodon-ui/pro';
import { Icon, Tooltip } from 'choerodon-ui';
import { IField } from '@/common/types';
import { IChoseFieldComponentProps } from '@/components/chose-field';
import { IIssueFilterFormProps } from '@/components/issue-filter-form';
import Search from './Search';
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
  reportDs: DataSet,
  dimension: IField[],
  choseComponentProps: IChoseFieldComponentProps,
  filterComponentProps: IIssueFilterFormProps,
}> = ({
  reportDs, dimension, filterComponentProps, choseComponentProps,
}) => (
  <div className={styles.condition}>
    <Part title="图表说明">
      <Form dataSet={reportDs} className={styles.form}>
        <TextArea
          name="description"
          autoSize
        />
      </Form>
    </Part>
    <Part title="数据设置" tip="您可以在此设置图表的分析维度、对比维度，以及统计单位。例如：分析维度为优先级，对比维度为迭代，统计单位为工作项计数。那么图表展示的就是各个优先级在各个迭代的工作项数量。">
      <Form dataSet={reportDs} className={styles.form}>
        <Select name="type" clearButton={false}>
          <Option value="line">基础折线图</Option>
          <Option value="pie">饼图</Option>
          <Option value="bar">柱状图</Option>
          <Option value="stackedBar">堆叠柱状图</Option>
        </Select>
        <Select name="row" clearButton={false}>
          {
              dimension.filter((item) => item.code !== reportDs.current?.get('column')).map((item) => (
                <Option value={item.code}>{item.name}</Option>
              ))
            }
        </Select>
        {
          reportDs.current?.get('type') === 'stackedBar' && (
            <Select name="column" clearButton={false}>
              {
              dimension.filter((item) => item.code !== reportDs.current?.get('row')).map((item) => (
                <Option value={item.code}>{item.name}</Option>
              ))
            }
            </Select>
          )
        }
        <Select name="unit" clearButton={false}>
          <Option value="storyPoints">故事点</Option>
          <Option value="quantity">工作项计数</Option>
        </Select>
      </Form>
    </Part>
    <Part title="数据过滤" tip="设置图表的数据范围。例如：您可以选择一部分迭代的工作项进行分析">
      <div className={styles.search}>
        <Search filterComponentProps={filterComponentProps} choseComponentProps={choseComponentProps} />
      </div>
    </Part>
  </div>
);

export default observer(Condition);
