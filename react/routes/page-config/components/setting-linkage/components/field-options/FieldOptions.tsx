import React, { useCallback, useEffect, useState } from 'react';
import { TextField, Button } from 'choerodon-ui/pro';
import { Icon } from 'choerodon-ui';
import { observer } from 'mobx-react-lite';
import { axios } from '@choerodon/boot';
import { IField, IPriority, IVersion } from '@/common/types';
import classNames from 'classnames';
import { getProjectId, getOrganizationId } from '@/utils/common';
import styles from './FieldOptions.less';

interface Props {
  field: IField
  onChange: (id: string, needPrepare?: boolean) => void
  currentOptionId: string | undefined
}
interface IOption {
  id: string
  value: string
}

const getOptionsConfig = ({
  fieldCode, fieldId, search, page = 0, size = 20,
}: {fieldCode: string, fieldId?: string, search?: string, page?: number, size?: number}) => {
  switch (fieldCode) {
    case 'priorityId': {
      return ({
        url: `/agile/v1/projects/${getProjectId()}/priority/list_by_org`,
        method: 'get',
        transformResponse: (response: any) => {
          try {
            const data = JSON.parse(response);
            return data.map((item: IPriority) => ({
              id: item.id,
              value: item.name,
            }));
          } catch (error) {
            return response;
          }
        },
      });
    }
    case 'fixVersion': {
      return ({
        url: `/agile/v1/projects/${getProjectId()}/product_version/names`,
        method: 'post',
        data: ['version_planning'],
        transformResponse: (response: any) => {
          try {
            const data = JSON.parse(response);
            return data.map((item: IVersion) => ({
              id: item.versionId,
              value: item.name,
            }));
          } catch (error) {
            return response;
          }
        },
      });
    }
    case 'influenceVersion': {
      return ({
        url: `/agile/v1/projects/${getProjectId()}/product_version/names`,
        method: 'post',
        data: [],
        transformResponse: (response: any) => {
          try {
            const data = JSON.parse(response);
            return data.map((item: IVersion) => ({
              id: item.versionId,
              value: item.name,
            }));
          } catch (error) {
            return response;
          }
        },
      });
    }
    default: {
      return ({
        method: 'get',
        url: `/agile/v1/projects/${getProjectId()}/field_value/${fieldId}/options`,
        params: {
          searchValue: search,
          page,
          size,
          organizationId: getOrganizationId(),
        },
      });
    }
  }
};

const FieldOptions: React.FC<Props> = ({
  field, onChange, currentOptionId,
}) => {
  const [options, setOptions] = useState<IOption[]>([]);
  const [search, setSearch] = useState('');
  const [totalPage, setTotalPage] = useState<number>(0);
  const [page, setPage] = useState<number>(1);
  const { system } = field;
  const getOptions = useCallback((filter?: string, p?: number) => {
    const newPage = p || page;
    axios(getOptionsConfig({
      fieldCode: field.fieldCode as string,
      fieldId: field.id,
      search: (filter || filter === null) ? filter : search,
      page: newPage,
    })).then((res: any) => {
      if (res.content) {
        if (newPage > 1) { // 大于第一页
          setOptions((preOptions) => [...preOptions, ...res.content]);
        } else {
          setOptions(res.content);
        }
        setPage(res.number + 1);
        setTotalPage(res.totalPages);
      } else {
        setOptions(res);
      }
    });
  }, [field.fieldCode, field.id, page, search]);

  useEffect(() => {
    getOptions();
  }, [getOptions]);

  useEffect(() => {
    if (!currentOptionId && options.length) {
      onChange(options[0].id, false);
    }
  }, [currentOptionId, onChange, options]);

  const handleSearch = useCallback((s) => {
    setSearch(s);
    setPage(1);
    setTotalPage(0);
    if (!system) {
      getOptions(s, 1);
    }
  }, [getOptions, system]);

  const handleLoadMore = useCallback(() => {
    getOptions(undefined, page + 1);
  }, [getOptions, page]);

  return (
    <div className={styles['option-list']}>
      <TextField
        value={search}
        onChange={handleSearch}
        prefix={<Icon type="search" style={{ color: 'rgba(0, 0, 0, 0.45)', marginLeft: 2 }} />}
        placeholder="请输入搜索内容"
        className={styles.search}
        style={{ display: 'block', margin: '0 20px 6px' }}
      />
      <div className={styles.options}>
        {options?.filter((option) => option.value.indexOf(search || '') > -1).map((option) => (
          <div
            role="none"
            className={classNames(styles['option-item'], {
              [styles['option-item-selected']]: option.id === currentOptionId,
            })}
            onClick={() => {
              onChange(option.id);
            }}
          >
            {option.value}
          </div>
        ))}
        {
        !system && page < totalPage && (
          <Button
            onClick={handleLoadMore}
            className={styles.loadMoreBtn}
          >
            <span>查看更多</span>
            <Icon type="baseline-arrow_right icon" style={{ marginRight: 2 }} />
          </Button>
        )
      }
        {
          !options?.filter((option) => option.value.indexOf(search || '') > -1).length && (
            <div className={styles.noContent}>{`${search ? '该搜索条件下暂无选项' : '暂无选项'}`}</div>
          )
        }
      </div>
    </div>
  );
};

export default observer(FieldOptions);
