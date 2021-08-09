/* eslint-disable react/button-has-type */
import React, { useState, useEffect } from 'react';
import {
  Page, Header, Content, stores, Permission, Breadcrumb,
} from '@choerodon/boot';
import { HeaderButtons } from '@choerodon/master';
import { Table, Button } from 'choerodon-ui/pro';
import AnimationLoading, { LoadingProvider, useLoading } from '@/components/Loading';
import { LoadingHiddenWrap } from '..';

const LeftChildren: React.FC = () => {
  const { change } = useLoading();
  // useEffect(() => {
  //   change('left', true, { allowSelfLoading: false });
  //   setTimeout(() => {
  //     change('left', false, { allowSelfLoading: false });
  //   }, 1200);
  // }, [change]);
  useEffect(() => {
    console.log('leftChildren render....');
  }, []);
  return (
    <div>
      <Button onClick={() => {
        change('left', true, { allowSelfLoading: true });

        setTimeout(() => {
          change('left', false, { allowSelfLoading: true });
        }, 300);
      }}
      >
        触发更新

      </Button>
      左子元素
    </div>
  );
};
const RightChildren: React.FC = () => {
  const { change } = useLoading();
  useEffect(() => {
    change('right', true);
    setTimeout(() => {
      change('right', false);
    }, 8000);
  }, [change]);
  return (
    <div>
      <Button onClick={() => {
        change('right', true);
        // actions?.change('provider', true);
        setTimeout(() => {
          change('right', false);
        }, 800);
      }}
      >
        触发更新

      </Button>
      右子元素
    </div>
  );
};
const ReleaseHome: React.FC = () => {
  const [glLoading, setGlLoading] = useState(false);
  const [data, setData] = useState<number[]>([]);
  const [leftData, setLeftData] = useState<number[]>([]);
  const [rightData, setRightData] = useState<number[]>([]);
  const [bottomData, setBottomData] = useState<string | false>();
  const loadRightData = () => {
    // actions?.change('left', true);

    setTimeout(() => {
      // actions?.change('left', false);

      setRightData(new Array(20).fill(0).map((i, index) => index + 8));
    }, 2000);
  };
  const loadLeftData = () => {
    setTimeout(() => {
      setLeftData(new Array(20).fill(0).map((i, index) => index + 9));
    }, 1000);
  };
  const loadData = () => {
    // setTimeout(() => {
    //   setData(new Array(20).fill(0).map((i, index) => index + 10));
    // }, 300);
    loadRightData();
    loadLeftData();
  };
  //   const [noDeliver, setNoDeliver] = useState<string>([]);
  useEffect(() => { loadData(); }, []);
  useEffect(() => {
    setGlLoading(true);
    setTimeout(() => {
      setGlLoading(false);
    }, 2500);
  }, []);
  return (
    <Page>
      <Header title="测试页面">
        <HeaderButtons
          items={[
            {
              name: '加载全部数据',
              icon: 'playlist_add',
              handler: () => {
                setData([]);
                setRightData([]);
                setLeftData([]);
                loadData();
              },
              display: true,
            },
            {
              name: '加载左边数据',
              icon: 'playlist_add',
              handler: () => {
                setLeftData([]);
                loadLeftData();
              },
              display: true,
            },
            {
              name: '单独加载底部数据',
              icon: 'playlist_add',
              handler: () => {
                setBottomData(false);
                setTimeout(() => {
                  setBottomData('999999999999');
                }, 1000);
              },
              display: true,
            },
          ]}
        />
      </Header>
      <Breadcrumb />
      <Content className="c7n-release-content">
        <LoadingProvider loading={glLoading} globalSingle>
          <div style={{ height: '500px', width: '100%' }}>
            <div style={{
              float: 'left', width: '50%', height: '100%', borderRight: '1px solid red', position: 'relative',
            }}
            >
              <AnimationLoading loadId="left" allowSelfLoading>
                <div>
                  <LoadingHiddenWrap>
                    <LeftChildren />
                  </LoadingHiddenWrap>
                  左边
                  {leftData.map((i) => <span>{`left-${i}`}</span>)}
                </div>
              </AnimationLoading>

            </div>
            <div style={{
              float: 'right', height: '100%', width: '40%', position: 'relative',
            }}
            >
              <AnimationLoading loadId="right">

                右边
                {/* <RightChildren /> */}
                {rightData.map((i) => <span>{`right-${i}`}</span>)}
              </AnimationLoading>
            </div>

          </div>
          <AnimationLoading noDeliverLoading loadId="bottom" loading={bottomData === false}>

            <div style={{ height: '120px' }}>
              内容一
              {bottomData}
            </div>

          </AnimationLoading>
        </LoadingProvider>
      </Content>
    </Page>
  );
};

export default ReleaseHome;
