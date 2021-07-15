import { useIssueSearchStore } from '@/components/issue-search';
import {
  Button, Dropdown,
} from 'choerodon-ui/pro';
import classNames from 'classnames';
import { observer } from 'mobx-react-lite';
import { find } from 'lodash';
import { runInAction } from 'mobx';
import { transformFilter } from '@/routes/Issue/stores/utils';
import { CustomReportSearchProps } from '@/routes/ReportHost/custom-report/components/ChartSearch/ChartSearch';
import ChoseField, { useChoseField } from '@/components/chose-field';
import React, { useState, useCallback, useEffect } from 'react';
import useClickOut from '@/hooks/useClickOut';
import { getSystemFields } from '@/stores/project/issue/IssueStore';
import useGetIssueSearchData from './useGetIssueSearchData';
import CustomSearchFields from './Search';
import './index.less';

const InjectCustomSearch: React.FC<CustomReportSearchProps> = ({ searchVO, setSearchVO, projectId }) => {
  const prefixCls = 'c7n-agile-inject-custom-search';
  const [fields, setFields] = useState([] as any[]);
  const [{ store: chosenStore }, choseFiledProps] = useChoseField({
    fields,
  });
  const issueSearchStore = useIssueSearchStore({
    projectId,
    transformFilter,
    defaultSearchVO: searchVO,
    // @ts-ignore
    getSystemFields: () => getSystemFields().filter((f) => !['contents'].includes(f.code)),
  });

  useEffect(() => {
    issueSearchStore.initChosenFields();
    issueSearchStore.loadCustomFields().then((res) => {
      runInAction(() => {
        setFields(issueSearchStore.getAllFields.filter((item: any) => !item.noDisplay && !item.defaultShow));
      });
    });
  }, [issueSearchStore]);

  const [hidden, setHidden] = useState(true);
  const handleClickOut = useCallback(() => {
    setHidden(true);
  }, []);
  const containerRef = useClickOut(handleClickOut);

  const handleChoseField = useCallback((key: string) => {
    const field = find(fields, { code: key });
    field && chosenStore.addChosenFields(key, field);
  }, [chosenStore, fields]);
  const { onChange, onClear } = useGetIssueSearchData({ store: issueSearchStore, onChoseField: handleChoseField, chosenStore });
  const handleChange = useCallback(({ code }: any, value: any) => {
    onChange(code, value);
  }, [onChange]);
  const handleQuery = () => {
    const newSearchVO = issueSearchStore.getCustomFieldFilters();
    setSearchVO(newSearchVO);
  };
  const handleClear = () => {
    onClear();
    setSearchVO(undefined as unknown as any);
  };
  return (
    <div className={prefixCls}>
      <Dropdown
        hidden={hidden}
        overlay={(
          <div
            ref={containerRef as any}
            role="none"
            onMouseDown={(e) => e.stopPropagation()}
            className={`${prefixCls}-menu`}
          >
            <CustomSearchFields className={`${prefixCls}-menu-top`} fields={issueSearchStore.chosenFields} onChange={handleChange} />
            <div className={`${prefixCls}-menu-bottom`}>
              <Button
                color={'primary' as any}
                onClick={() => {
                  handleQuery();
                  // handleChange()
                  setHidden(true);
                }}
                style={{ marginLeft: '.1rem' }}
              >
                查询
              </Button>
              <Button onClick={handleClear}>重置</Button>
              <ChoseField {...choseFiledProps} />
            </div>
          </div>
        )}
        trigger={['click'] as any}
      >

        <Button
          icon="filter2"
          className={classNames(`${prefixCls}-search-btn`, { [`${prefixCls}-search-btn-active`]: issueSearchStore.isHasFilter })}
          onClick={(e) => {
            e.nativeEvent.stopImmediatePropagation();
            setHidden((old) => !old);
          }}
        />
      </Dropdown>
    </div>
  );
};
export default observer(InjectCustomSearch);
