/* eslint-disable no-nested-ternary */
import React, {
  useMemo, useCallback, useState, useEffect,
} from 'react';
import { runInAction } from 'mobx';
import { observer } from 'mobx-react-lite';
import {
  Page, Header, Content, Breadcrumb, HeaderButtons,
} from '@choerodon/boot';
import {
  Button, DataSet, TextField, Modal, Form,
  Spin,
} from 'choerodon-ui/pro';
import { EmptyPage } from '@choerodon/components';

import { ButtonColor } from 'choerodon-ui/pro/lib/button/enum';
import { FieldType } from 'choerodon-ui/pro/lib/data-set/enum';
import {
  customReportApi, fieldApi, ICreateData, pageConfigApi,
} from '@/api';
import { IField } from '@/common/types';

import to from '@/utils/to';
import LINK_URL from '@/constants/LINK_URL';
import { SearchVOToFilter } from '@/components/issue-search/utils';
import emptyPic from '@/assets/image/NoData.svg';
import { useIssueFilterForm } from '@/components/issue-filter-form';
import { getCustomFieldFilters } from '@/components/issue-export/utils';
import { getSystemFields } from '@/stores/project/issue/IssueStore';
import { useChoseField } from '@/components/chose-field';
import { getTransformSystemFilter } from '@/routes/Issue/components/ExportIssue/utils';
import pic from './NoData.svg';
import styles from './Report.less';
import Condition from '../components/Condition';
import useReport from '../components/Chart/useReport';
import Chart from '../components/Chart';
import { IChartData } from '../components/Chart/utils';
import Table from '../components/Table';
import ChartSearch from '../components/ChartSearch';
import { LoadingProvider } from '@/components/Loading';
import useIsWaterfall from '@/hooks/useIsWaterfall';

interface Props {
  chartId?: string
}

export interface IChartRes extends ICreateData {
  id: string,
  customChartData: IChartData[],
}

const CustomReport: React.FC<Props> = (props) => {
  const { isWaterfallAgile } = useIsWaterfall();
  // @ts-ignore
  const chartId = props.match.params.id;
  const [mode, setMode] = useState<'create' | 'edit' | 'read'>(chartId ? 'read' : 'create');
  const [loading, setLoading] = useState<boolean>(chartId || false); // 编辑时默认loading
  const [lost, setLost] = useState<boolean>(false);
  const [chartRes, setChartRes] = useState<null | IChartRes>(null);
  const [dimension, setDimension] = useState<IField[]>([]);
  const [customChartList, setCustomChartList] = useState<{ title: string, path: string }[]>([]);
  const [customFields, setCustomFields] = useState<IField[]>([]);
  const [hasGetCustomFields, setHasGetCustomFields] = useState<boolean>(false);
  const [expand, setExpand] = useState<boolean>(true);

  useEffect(() => {
    pageConfigApi.load().then((res: { content: IField[] }) => {
      setDimension(res.content.filter((item) => ['single', 'checkbox', 'multiple', 'radio', 'member', 'multiMember'].includes(item.fieldType) && !(item.contexts.length === 1 && item.contexts[0] === 'backlog')));
    });
  }, []);

  useEffect(() => {
    if (chartId) {
      customReportApi.getCustomReports().then((res: IChartRes[]) => {
        if (res.length) {
          setCustomChartList(res.filter((item) => item.id !== chartId).map((item) => ({
            title: item.name,
            path: `/agile/charts/${item.id}`,
          })));
        }
      });
    }
  }, [chartId]);

  const checkTitle = useCallback(async (value, name, record) => {
    if (!value || value === chartRes?.name) {
      return true;
    }
    const res = await customReportApi.checkName(value);
    if (!res) {
      return true;
    }
    return '图表标题重复';
  }, [chartRes?.name]);
  const reportDs = useMemo(() => new DataSet({
    autoCreate: true,
    fields: [{
      name: 'title',
      label: '图表标题',
      type: 'string' as FieldType,
      maxLength: 16,
      required: true,
      validator: checkTitle,
      defaultValue: '未命名自定义图表',
    }, {
      name: 'description',
      label: '图表描述',
      type: 'string' as FieldType,
      maxLength: 50,
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

  const handleClickEdit = useCallback(() => {
    setMode('edit');
  }, []);

  const handleClickDelete = useCallback(async () => {
    if (chartId) {
      Modal.open({
        key: Modal.key(),
        title: '确认删除',
        children: (
          <div>
            {`您确定要删除“${chartRes?.name}”吗？`}
          </div>
        ),
        onOk: async () => {
          await customReportApi.deleteChart(chartId);
          to(LINK_URL.report);
          return true;
        },
      });
    }
  }, [chartId, chartRes?.name]);

  const back = useCallback(() => {
    to(LINK_URL.report);
  }, []);

  useEffect(() => {
    const getCustomFields = async () => {
      const fields = await fieldApi.getCustomFields(isWaterfallAgile ? '' : undefined);
      setCustomFields(fields);
      setHasGetCustomFields(true);
    };
    getCustomFields();
  }, [isWaterfallAgile]);

  const fields = useMemo(() => {
    const allField = [...customFields, ...getSystemFields().filter((field) => !field.archive && !field.noDisplay)];
    if (isWaterfallAgile) {
      const issueTypeField = allField.find((field) => field.code === 'issueTypeId');
      issueTypeField && Object.assign(issueTypeField, { otherComponentProps: { applyType: '' } });
    }
    return allField;
  }, [customFields, isWaterfallAgile]);

  const [choseDataProps, choseComponentProps] = useChoseField({
    fields,
  });

  const { store: choseFieldStore } = choseDataProps;

  const [filterData, filterComponentProps] = useIssueFilterForm({
    fields,
    value: choseFieldStore.getAllChosenField,
    events: {
      afterDelete: (item) => {
        choseFieldStore.delChosenFields(item.code);
      },
    },
  });

  const search = hasGetCustomFields ? getCustomFieldFilters(choseFieldStore.getAllChosenField, filterData.dataSet.current!, getTransformSystemFilter) : undefined;

  // eslint-disable-next-line react-hooks/exhaustive-deps
  const searchVO = useMemo(() => search, [JSON.stringify(search)]);
  const chartType = reportDs.current?.get('type');
  const analysisField = reportDs.current?.get('row');
  const comparedField = reportDs.current?.get('column');
  const statisticsType = reportDs.current?.get('unit');
  const configMemo = useMemo(() => ({
    chartType,
    statisticsType,
    analysisField,
    comparedField,
    analysisFieldPredefined: (analysisField && dimension.find((item) => item.code === analysisField)?.system),
    comparedFieldPredefined: comparedField && dimension.find((item) => item.code === comparedField)?.system,
    searchVO,
  }), [analysisField, chartType, comparedField, dimension, searchVO, statisticsType]);
  const [searchProps, chartProps] = useReport(configMemo);
  const {
    data,
  } = chartProps;

  const initSetting = useCallback((res) => {
    runInAction(() => {
      if (res?.searchJson && JSON.parse(res?.searchJson)) {
        const searchVOFiltered = SearchVOToFilter(JSON.parse(res?.searchJson || {}));
        for (const [key, value] of Object.entries(searchVOFiltered)) {
          const field = fields.find((item: IField) => item.code === key || item.id === key);
          if (field && value) {
            if ((Array.isArray(value) && value.length) || !Array.isArray(value)) {
              choseFieldStore.addChosenFields(field.code, field, value);
            }
          }
        }
      }
      reportDs.current?.set('title', res?.name);
      reportDs.current?.set('description', res?.description);
      reportDs.current?.set('type', res?.chartType);
      reportDs.current?.set('row', res?.analysisField);
      reportDs.current?.set('column', res?.comparedField);
      reportDs.current?.set('unit', res?.statisticsType);
    });
  }, [choseFieldStore, fields, reportDs]);

  const refresh = useCallback(async () => {
    if (chartId && dimension.length && hasGetCustomFields) {
      setLoading(true);
      const res: IChartRes = await customReportApi.getChartAllDataById(chartId);
      setLoading(false);
      if (!dimension.find((item) => item.code === res.analysisField) || (res.comparedField && !dimension.find((item) => item.code === res.comparedField))) {
        setLost(true);
      } else {
        setChartRes(res);
        initSetting(res);
      }
    }
  }, [chartId, dimension, hasGetCustomFields, initSetting]);

  useEffect(() => {
    refresh();
  }, [refresh]);

  const handleSave = useCallback(async () => {
    const validate = await reportDs.validate();
    if (validate) {
      const submitData: ICreateData = {
        searchJson: searchVO ? JSON.stringify(searchVO) : undefined,
        name: reportDs.current?.get('title'),
        description: reportDs.current?.get('description'),
        chartType,
        statisticsType,
        analysisField,
        comparedField,
        analysisFieldPredefined: analysisField && dimension.find((item) => item.code === analysisField)?.system as boolean,
        comparedFieldPredefined: comparedField && dimension.find((item) => item.code === comparedField)?.system,
        objectVersionNumber: chartRes?.objectVersionNumber,
      };
      const res = mode === 'create' ? await customReportApi.createChart(submitData) : await customReportApi.updateChart(chartId as string, submitData);
      if (mode === 'create') {
        to(`/agile/charts/${res.id}`);
      } else {
        setMode('read');
        refresh();
      }
    }
    setExpand(true);
  }, [reportDs, searchVO, chartType, statisticsType, analysisField, comparedField, dimension, chartRes?.objectVersionNumber, mode, chartId, refresh]);

  const handleCancel = useCallback(() => {
    if (mode === 'create') {
      reportDs.current?.reset();
      back();
    } else {
      initSetting(chartRes);
      setMode('read');
    }
  }, [back, chartRes, initSetting, mode, reportDs]);

  const handleClickSetting = useCallback(() => {
    if (chartId && lost) {
      setMode('edit');
    }
    setExpand(true);
  }, [chartId, lost]);

  return (
    <Page>
      <Header>
        <HeaderButtons items={(mode !== 'read' || !chartRes?.id) ? [] : [...(customChartList.length ? [{
          display: true,
          name: '切换图表',
          groupBtnItems: customChartList.map((item) => ({
            display: true,
            name: item.title,
            handler: () => {
              to(item.path);
            },
          })),
        }] : []), {
          display: true,
          name: '编辑图表',
          icon: 'edit-o',
          handler: handleClickEdit,
        }, {
          display: true,
          name: '删除图表',
          icon: 'delete_sweep-o',
          handler: handleClickDelete,
        }, {
          name: '返回',
          icon: 'arrow_back',
          iconOnly: true,
          display: true,
          handler: back,
        }, {
          name: '刷新',
          icon: 'refresh',
          iconOnly: true,
          handler: refresh,
          display: true,
        }]}
        />
      </Header>
      <Breadcrumb title={chartId ? (mode === 'read' ? chartRes?.name : '编辑自定义图表') : '创建自定义图表'} />
      <Content style={{ padding: 0 }} className={styles.content}>
        <LoadingProvider loading={loading} style={{ height: '100%' }}>
          <div
            className={styles.report}
            style={{
              height: mode === 'read' ? 'auto' : '100%',
              overflowY: mode === 'read' ? 'auto' : 'hidden',
            }}
          >
            {mode !== 'read' && (
              <div className={styles.header}>
                <Form className={styles.titleForm}>
                  <TextField dataSet={reportDs} name="title" placeholder="请输入图表标题" style={{ width: 400 }} />
                </Form>
              </div>
            )}
            {
              mode === 'read' && (
                <ChartSearch {...searchProps} />
              )
            }
            <div className={styles.main}>
              <div className={styles.left} key={`${chartType}-${statisticsType}-${analysisField}-${comparedField}`}>
                {
                  !loading && (
                    <>
                      {
                        ((!statisticsType || !chartType || !analysisField || (chartType === 'stackedBar' && !comparedField)) || lost) ? (
                          <EmptyPage
                            image={pic}
                            description={lost ? (
                              <>
                                暂无分析维度，请进行
                                <span
                                  style={{
                                    color: '#5365EA',
                                    cursor: 'pointer',
                                  }}
                                  role="none"
                                  onClick={handleClickSetting}
                                >
                                  数据设置
                                </span>
                              </>
                            ) : (
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
                          <>
                            {
                              (data || []).length > 0 ? (
                                <>
                                  <Chart {...chartProps} key={`${chartType}-${statisticsType}-${analysisField}-${comparedField}`} />
                                  <Table {...chartProps} analysisField={analysisField} dimension={dimension} />
                                </>
                              ) : (
                                <>
                                  {
                                    data !== null && (
                                      <EmptyPage
                                        image={emptyPic}
                                        description="当前暂无数据"
                                      />
                                    )
                                  }
                                </>
                              )
                            }
                          </>
                        )
                      }
                    </>
                  )
                }
              </div>
              {
                mode !== 'read' && (
                  <div
                    className={styles.right}
                    style={{
                      width: expand ? 320 : 'unset',
                    }}
                  >
                    <div className={styles.expand_btn_container}>
                      <Button
                        icon={expand ? 'last_page' : 'first_page'}
                        className={styles.expand_btn}
                        onClick={handleExpandChange}
                      />
                    </div>
                    {
                      expand && <Condition filterComponentProps={filterComponentProps} choseComponentProps={choseComponentProps} reportDs={reportDs} dimension={dimension} />
                    }
                  </div>
                )
              }
            </div>
            {
              mode !== 'read' && (
                <div className={styles.footer}>
                  <Button color={'primary' as ButtonColor} onClick={handleSave}>保存</Button>
                  <Button onClick={handleCancel}>取消</Button>
                </div>
              )
            }
          </div>
        </LoadingProvider>
      </Content>
    </Page>
  );
};

export default observer(CustomReport);
