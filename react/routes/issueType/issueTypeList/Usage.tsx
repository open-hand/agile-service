import React, { useEffect, useState, useCallback } from 'react';
import { unstable_batchedUpdates as batchedUpdates } from 'react-dom';
import { observer } from 'mobx-react-lite';
import { Modal, Button } from 'choerodon-ui/pro';
import { IModalProps } from '@/common/types';
import { Choerodon } from '@choerodon/boot';
import Record from 'choerodon-ui/pro/lib/data-set/Record';
import { issueTypeApi } from '@/api';
import { Loading } from '@/components';
import ProjectTag from '@/components/tag/project-tag';
import styles from './Usage.less';

interface Props {
  modal?: IModalProps,
  record: Record | null | undefined,
}

interface IUsage {
  enabled: boolean
  id: string
  imageUrl: string
  name: string
}

interface IRes {
  totalElements: number,
  number: number, // 从0开始
  list: IUsage[],
  size: number,
}

const Usage: React.FC<Props> = ({ modal, record }) => {
  const [page, setPage] = useState<number>(0);
  const [res, setRes] = useState<IRes>({
    totalElements: 0,
    number: 0,
    list: [],
    size: 15,
  });
  const [list, setList] = useState<IUsage[]>([{
    id: '1529',
    imageUrl: 'https://minio.choerodon.com.cn/hzero-iam/7/9f3345bdadb34d77b4055840b417b67f@file_fa123f7e8bd34dd4992b5407c277a1df_apple-touch-icon-144.png',
    name: 'Choerodon持续交付与测试',
    enabled: true,
  }, {
    id: '1529',
    imageUrl: 'https://minio.choerodon.com.cn/hzero-iam/7/9f3345bdadb34d77b4055840b417b67f@file_fa123f7e8bd34dd4992b5407c277a1df_apple-touch-icon-144.png',
    name: 'Choerodon持续交付与测试',
    enabled: false,
  }, {
    id: '1529',
    imageUrl: 'https://minio.choerodon.com.cn/hzero-iam/7/9f3345bdadb34d77b4055840b417b67f@file_fa123f7e8bd34dd4992b5407c277a1df_apple-touch-icon-144.png',
    name: 'Choerodon持续交付与测试',
    enabled: true,
  }, {
    id: '1529',
    imageUrl: 'https://minio.choerodon.com.cn/hzero-iam/7/9f3345bdadb34d77b4055840b417b67f@file_fa123f7e8bd34dd4992b5407c277a1df_apple-touch-icon-144.png',
    name: 'Choerodon持续交付与测试',
    enabled: true,
  }]);
  const [loading, setLoading] = useState<boolean>(false);

  const getUsage = useCallback((p = 0) => {
    setLoading(true);
    issueTypeApi.getUsage(p || page).then((data: IRes) => {
      batchedUpdates(() => {
        setLoading(false);
        setRes(data);
        setList([...list, ...(res.list || [])]);
      });
    }).catch(() => {
      setLoading(false);
      Choerodon.prompt('加载失败');
    });
  }, [list, page, res.list]);

  useEffect(() => {
    getUsage();
  }, [getUsage]);

  const handleClickMore = useCallback(() => {
    setPage(page + 1);
    getUsage(page + 1);
  }, [getUsage, page]);

  return (
    <div className={styles.usage}>
      <Loading loading={loading} />
      {
        (list && list.length > 0) ? (
          <div className={styles.list}>
            {
              list.map((item) => (
                // <div className={styles.usageItem}>woshiyigexianmu1</div>
                <div className={styles.usageItem}>
                  <ProjectTag
                    showText
                    data={{
                      id: 1529,
                      imageUrl: 'https://minio.choerodon.com.cn/hzero-iam/7/9f3345bdadb34d77b4055840b417b67f@file_fa123f7e8bd34dd4992b5407c277a1df_apple-touch-icon-144.png',
                      name: 'Choerodon持续交付与测试',
                    }}
                  />
                  <div className={`${styles.status} ${styles[`status_${!!(item.enabled)}`]}`}>
                    {
                      item.enabled ? '启用' : '停用'
                    }
                  </div>
                </div>

              ))
            }
            {
              (res.number + 1) * res.size < res.totalElements && (
                <div className={styles.more} role="none" onClick={handleClickMore}>查看更多</div>
              )
            }
          </div>
        ) : '没有项目使用该问题类型'
      }
    </div>
  );
};

const ObserverUsage = observer(Usage);

const openUsage = (props: Props) => {
  Modal.open({
    drawer: true,
    key: 'usage',
    title: '使用情况',
    style: {
      width: 380,
    },
    className: styles.batchDeleteModal,
    children: <ObserverUsage {...props} />,
    okText: '关闭',
    footer: (okBtn: Button) => (
      <div>
        {okBtn}
      </div>
    ),
  });
};

export default openUsage;
