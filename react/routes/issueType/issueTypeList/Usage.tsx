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
  creationDate: string
}

interface IRes {
  totalElements: number,
  number: number, // 从0开始
  list: IUsage[],
  size: number,
}

const Usage: React.FC<Props> = ({ modal, record }) => {
  const [page, setPage] = useState<number>(1);
  const [res, setRes] = useState<IRes>({
    totalElements: 0,
    number: 0,
    list: [],
    size: 15,
  });
  const [list, setList] = useState<IUsage[]>([]);
  const [loading, setLoading] = useState<boolean>(false);

  const getUsage = useCallback(() => {
    setLoading(true);
    issueTypeApi.orgGetUsage(record?.get('id'), page).then((data: IRes) => {
      batchedUpdates(() => {
        setLoading(false);
        setRes(data);
        setList((arr) => [...arr, ...(data.list || [])]);
      });
    }).catch(() => {
      setLoading(false);
      Choerodon.prompt('加载失败');
    });
  }, [page, record]);

  useEffect(() => {
    getUsage();
  }, [getUsage]);

  const handleClickMore = useCallback(() => {
    setPage(page + 1);
  }, [page]);

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
                    data={item}
                    size={28}
                    textStyle={{
                      color: 'rgba(0, 0, 0, 0.85)',
                      fontSize: 14,
                      marginLeft: 1,
                      height: 28,
                      lineHeight: '28px',
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
        ) : (
          <>
            {
                !loading ? '没有项目使用该工作项类型' : ''
              }
          </>
        )
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
