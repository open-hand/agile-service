// 缺陷排行榜
import React, { useContext, useEffect, useState } from 'react';
import { List } from 'choerodon-ui';
import { Spin, Form, Select } from 'choerodon-ui/pro';
import classnames from 'classnames';
import UserTag from '@/components/tag/user-tag';
import SwitchTabs from '../SwitchTabs';
import EmptyBlock from '../EmptyBlock';
import Store from '../../stores';
import firstSvg from '../../image/first.svg';
import secondSvg from '../../image/second.svg';
import thirdSvg from '../../image/third.svg';
import emptyListPic from '../../image/empty_list.svg';
import './index.less';

const rankImg = [firstSvg, secondSvg, thirdSvg];

const BugRankList = () => {
  const { bugRankDataSet, bugRankHandleDataSet } = useContext(Store);
  const [listData, setListData] = useState(null);

  useEffect(() => {
    queryList();
  }, []);

  const queryList = async () => {
    const { type, environment } = bugRankHandleDataSet.current.toData();
    bugRankDataSet.setQueryParameter('environment', environment);
    bugRankDataSet.setQueryParameter('type', type);
    await bugRankDataSet.query();
    setListData(bugRankDataSet.toData());
  };

  return (
    <div className="bug-rank-container">
      <div className="chart-handle">
        <span className="chart-title">缺陷排行榜</span>
        <SwitchTabs
          dataSet={bugRankHandleDataSet}
          field="type"
          onChange={() => queryList()}
          style={{ flexShrink: 0, height: '37px', marginLeft: 'auto' }}
        />
        <Form dataSet={bugRankHandleDataSet} style={{ width: '130px', marginLeft: '15px' }}>
          <Select
            name="environment"
            clearButton={false}
            onChange={() => queryList()}
          />
        </Form>
      </div>
      <Spin dataSet={bugRankDataSet} style={{ height: '510px' }}>
        {
          (listData && listData.length > 0) && (
            <div className="list-container">
              <List
                dataSource={listData}
                renderItem={({
                  loginName, realName, name, responsibleId, imageUrl, bugCount,
                }, index) => (
                  <List.Item
                    key={loginName}
                  >
                    <List.Item.Meta
                      description={(
                        <div className="list-item">
                          <div className={classnames('rank-number', { 'rank-number-first': index === 0 }, { 'rank-number-sec': index === 1 }, { 'rank-number-third': index === 2 })}>
                            <span>
                              {index + 1}
                            </span>
                          </div>
                          <div className="rank-img">
                            <div className="rank-img-container">
                              <UserTag
                                size={34}
                                showText={false}
                                data={{
                                  id: responsibleId,
                                  tooltip: name,
                                  loginName,
                                  realName,
                                  imageUrl,
                                }}
                              />
                              {[0, 1, 2].includes(index) && <img className="rank-img-container-crown" src={rankImg[index]} />}
                            </div>
                          </div>
                          <div className="rank-text rank-name">{realName || name}</div>
                          <div className="rank-text rank-count">{`${bugCount}个`}</div>
                        </div>
                      )}
                    />
                  </List.Item>
                )}
              />
            </div>
          )
        }
        {
          (Array.isArray(listData) && listData.length === 0) && (
            <EmptyBlock
              pic={emptyListPic}
              height={510}
              des="当前暂无数据"
            />
          )
        }
      </Spin>
    </div>
  );
};

export default BugRankList;
