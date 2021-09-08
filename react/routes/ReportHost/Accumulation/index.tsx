import React from 'react';
import {
  Page, Header, Content, Breadcrumb, HeaderButtons,
} from '@choerodon/boot';
import Accumulation from '@/components/charts/accumulation';
import AccumulationSearch from '@/components/charts/accumulation/search';
import useAccumulationReport from '@/components/charts/accumulation/useAccumulationReport';
import pic from '@/assets/image/NoData.svg';
import BackBtn from '../back-btn';
import NoDataComponent from '../Component/noData';
import SwithChart from '../Component/switchChart';
import { LoadingHiddenWrap, LoadingProvider } from '@/components/Loading';

const AccumulationReport: React.FC = () => {
  const [searchProps, props, refresh] = useAccumulationReport({ defaultLoading: true });
  const renderContent = () => {
    const { data } = props;
    if (!data.length) {
      return (
        <NoDataComponent title="问题" links={[{ name: '所有问题', link: '/agile/work-list/issue' }]} img={pic} />
      );
    }
    return (
      <div className="c7n-accumulation-report" style={{ flexGrow: 1, height: '100%' }}>
        <Accumulation {...props} />
      </div>
    );
  };

  return (
    <Page>
      <Header
        title="累积流量图"
      >
        <HeaderButtons
          items={[{
            name: '切换',
            element: <SwithChart
              // @ts-ignore
              current="accumulation"
            />,
            display: true,
          }, {
            name: '返回',
            element: <BackBtn />,
            display: true,
          }, {
            name: '刷新',
            icon: 'refresh',
            iconOnly: true,
            handler: () => {
              refresh();
            },
            display: true,
          }]}
        />

      </Header>
      <Breadcrumb title="累积流量图" />
      <Content
        style={{
          display: 'flex',
          flexDirection: 'column',
        }}
      >
        <LoadingProvider
          loading={props.loading}
          loadId="Accumulation"
        >
          <div className="c7n-accumulation-filter">
            <AccumulationSearch {...searchProps} float />
          </div>
          <LoadingHiddenWrap>
            {renderContent()}
          </LoadingHiddenWrap>
        </LoadingProvider>
      </Content>
    </Page>
  );
};
export default AccumulationReport;
