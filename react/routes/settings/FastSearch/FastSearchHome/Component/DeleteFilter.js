import React, {
  Component, useState, useEffect, useImperativeHandle,
} from 'react';
import { Icon } from 'choerodon-ui/pro';
import { stores } from '@choerodon/boot';
import { quickFilterApi } from '@/api';

const { AppState } = stores;

const DeleteComponent = (props) => {
  const [filter, setFilter] = useState({});
  const [loading, setLoading] = useState(false);
  const [confirmShow, setConfirmShow] = useState(false);

  const init = () => {
    setFilter(props.filter || {});
  };

  const deleteFilter = () => {
    setLoading(true);
    quickFilterApi.delete(filter.filterId).then((res) => {
      setLoading(false);
      props.onOk();
    })
      .catch((error) => {
        setLoading(false);
      });
  };
  useImperativeHandle(props.forwardref, () => (
    {
      handleDelete: () => {
        deleteFilter();
      },
    }));

  useEffect(init, []);

  return (
    <div>
      删除后将无法使用该快速筛选，如果只是想要改变某些条件可以修改快速筛选。
    </div>
  );
};

export default DeleteComponent;
