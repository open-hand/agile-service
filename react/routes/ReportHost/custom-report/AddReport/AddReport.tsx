import React, {
  useMemo, useCallback, useState, useRef,
} from 'react';
import { observer } from 'mobx-react-lite';
import {
  Page, Header, Content, Breadcrumb, HeaderButtons,
} from '@choerodon/boot';
import {
  Button, DataSet, TextField, Form,
} from 'choerodon-ui/pro';
import { EmptyPage } from '@choerodon/components';
import { Icon } from 'choerodon-ui';
import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import { customReportApi } from '@/api';
import { IField } from '@/common/types';
import { useWhyDidYouUpdate } from 'ahooks';
import pic from './NoData.svg';
import styles from './AddReport.less';
import Condition from '../components/Condition';
import useReport from '../components/Chart/useReport';
import Chart from '../components/Chart';

const AddReport = () => {
  const conditionRef = useRef<{
    rowColumnOptions: IField[]
  } | null>(null);
  const [expand, setExpand] = useState<boolean>(true);
  const checkTitle = useCallback(async (value, name, record) => {
    const res = await customReportApi.checkName(value);
    if (!res) {
      return true;
    }
    return '报表标题重复';
  }, []);
  const addReportDs = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'title',
      label: '图表标题',
      type: 'string' as FieldType,
      maxLength: 16,
      required: true,
      validator: checkTitle,
    }, {
      name: 'description',
      label: '图表描述',
      type: 'string' as FieldType,
      maxLength: 100,
    }, {
      name: 'type',
      label: '图表类型',
      type: 'string' as FieldType,
      required: true,
    }, {
      name: 'row',
      label: '分析维度(表格行)',
      type: 'string' as FieldType,
      required: true,
    }, {
      name: 'column',
      label: '对比维度(表格列)',
      type: 'string' as FieldType,
      dynamicProps: {
        required: ({ record }) => record?.get('type') === 'stackedBar',
      },
    }, {
      name: 'unit',
      label: '图表单位',
      type: 'string' as FieldType,
      required: true,
    }],
    events: {
      update: ({
        // @ts-ignore
        name, value, oldValue, record,
      }) => {
        if (name === 'type' && oldValue === 'stackedBar') {
          record?.set('column', undefined);
        }
      },
    },
  }), [checkTitle]);

  const handleExpandChange = useCallback(() => {
    setExpand(!expand);
  }, [expand]);

  const chartType = addReportDs.current?.get('type');
  const analysisField = addReportDs.current?.get('row');
  const comparedField = addReportDs.current?.get('column');
  const statisticsType = addReportDs.current?.get('unit');
  const maxShow = expand ? 18 : 12;
  const configMemo = useMemo(() => ({
    chartType,
    statisticsType,
    analysisField,
    comparedField,
    searchVO: undefined,
    analysisFieldPredefined: analysisField && conditionRef.current?.rowColumnOptions.find((item) => item.code === analysisField)?.system,
    comparedFieldPredefined: comparedField && conditionRef.current?.rowColumnOptions.find((item) => item.code === comparedField)?.system,
  }), [analysisField, chartType, comparedField, statisticsType]);
  const [, chartProps] = useReport(configMemo, maxShow);
  const { data } = chartProps;
  useWhyDidYouUpdate('useWhyDidYouUpdateComponent', chartProps);
  // console.log(chartProps);
  return (
    <Page>
      <Header />
      <Breadcrumb />
      <Content style={{ padding: 0 }}>
        <div className={styles.addReport}>
          <div className={styles.header}>
            {/* <Form dataSet={addReportDs}>
              <TextField name="title" placeholder="图表标题" />
            </Form> */}
            <TextField dataSet={addReportDs} name="title" placeholder="图表标题" />
          </div>
          <div className={styles.main}>
            <div className={styles.left}>
              {
                !data?.length ? (
                  <EmptyPage
                    image={pic}
                    description={(
                      <>
                        当前暂无自定义图表，您可以通过
                        <span
                          style={{
                            color: '#5365EA',
                            cursor: 'pointer',
                          }}
                          role="none"
                          onClick={() => {
                            if (!expand) {
                              setExpand(true);
                            }
                          }}
                        >
                          数据设置
                        </span>
                        添加默认筛选项来创建图表。
                      </>
                )}
                  />
                ) : (
                  <Chart {...chartProps} key={`${chartType}-${statisticsType}-${analysisField}-${comparedField}`} />
                )
              }
            </div>
            <div
              className={styles.right}
              style={{
                width: expand ? 320 : 'unset',
              }}
            >
              <Button
                icon={expand ? 'last_page' : 'first_page'}
                className={styles.expand_btn}
                onClick={handleExpandChange}
              />
              {
                expand && <Condition addReportDs={addReportDs} conditionRef={conditionRef} />
              }
            </div>
          </div>
          <div className={styles.footer}>
            <Button color={'primary' as ButtonColor}>保存</Button>
            <Button>取消</Button>
          </div>
        </div>
      </Content>
    </Page>
  );
};

export default observer(AddReport);
