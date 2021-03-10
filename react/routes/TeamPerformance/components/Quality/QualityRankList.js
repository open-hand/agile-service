// 代码质量排行榜
import React, { useContext, useEffect, useState } from 'react';
import { List } from 'choerodon-ui';
import {
  Spin, Stores, Tooltip, Form, Select,
} from 'choerodon-ui/pro';
import classnames from 'classnames';
import UserTag from '@/components/tag/user-tag';
import EmptyBlock from '../EmptyBlock';
import Store from '../../stores';
import firstSvg from '../../image/first.svg';
import secondSvg from '../../image/second.svg';
import thirdSvg from '../../image/third.svg';
import emptyDataPic from '../../image/empty_list.svg';
import './index.less';

const rankImg = [firstSvg, secondSvg, thirdSvg];

const QualityRankList = () => {
  const { qualityRankHandleDataSet, qualityRankDataSet, projectId } = useContext(Store);
  const [listData, setListData] = useState([]);

  useEffect(() => {
    queryServices();
  }, []);

  // 查询应用服务
  const queryServices = async () => {
    const serviceData = await Stores.LookupCodeStore.fetchLookupData({
      url: `/devops/v1/projects/${projectId}/app_service/list_by_active`,
      method: 'get',
    });
    if (Array.isArray(serviceData) && serviceData.length !== 0) {
      qualityRankHandleDataSet.current.set('appServiceId', serviceData[0].id);
      queryList();
    }
  };

  const queryList = async () => {
    qualityRankDataSet.setQueryParameter('appServiceId', qualityRankHandleDataSet.current.get('appServiceId'));
    await qualityRankDataSet.query();
    setListData(qualityRankDataSet.toData());
  };

  return (
    <div className="quality-rank-container">
      <div className="chart-handle">
        <span className="chart-title">代码质量排行榜</span>
        <Form dataSet={qualityRankHandleDataSet} style={{ marginLeft: '35px', width: '250px' }}>
          <Select
            name="appServiceId"
            clearButton={false}
            onChange={() => queryList()}
          />
        </Form>
      </div>
      <Spin dataSet={qualityRankDataSet} style={{ height: '510px' }}>
        {
          (listData && listData.length > 0) && (
            <div className="list-container">
              <List
                dataSource={listData}
                renderItem={({
                  loginName, realName, userId, imageUrl, email, description, name,
                }, index) => (
                  <List.Item
                    key={loginName}
                  >
                    <List.Item.Meta
                      description={
                        (
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
                                    id: userId,
                                    tooltip: name,
                                    loginName,
                                    realName,
                                    imageUrl,
                                  }}
                                />
                                {[0, 1, 2].includes(index) && <img className="rank-img-container-crown" src={rankImg[index]} />}
                              </div>
                            </div>
                            <div className="rank-text rank-name-quality">{realName || '未分配'}</div>
                            <Tooltip title={email}>
                              <div className="rank-text rank-email">{email}</div>
                            </Tooltip>
                            <Tooltip title={description}>
                              <div className="rank-text rank-des">{description}</div>
                            </Tooltip>
                          </div>
                        )
                      }
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
              pic={emptyDataPic}
              height={510}
              des="当前暂无数据"
            />
          )
        }
      </Spin>
    </div>
  );
};

export default QualityRankList;
