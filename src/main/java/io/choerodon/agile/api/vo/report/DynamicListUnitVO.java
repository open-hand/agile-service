package io.choerodon.agile.api.vo.report;

import java.util.List;

import io.choerodon.agile.api.vo.SearchVO;
import org.hzero.core.base.BaseConstants;
import org.springframework.util.Assert;

/**
 * @author jiaxu.cui@hand-china.com 2020/9/18 下午2:32
 */
public class DynamicListUnitVO extends ReportUnitVO {

    @Override
    public void validate() {
        super.validate();
        Assert.notNull(searchVO, BaseConstants.ErrorCode.DATA_INVALID);
    }

    private List<String> colList;

    private SearchVO searchVO;

    public List<String> getColList() {
        return colList;
    }

    public void setColList(List<String> colList) {
        this.colList = colList;
    }

    public SearchVO getSearchVO() {
        return searchVO;
    }

    public void setSearchVO(SearchVO searchVO) {
        this.searchVO = searchVO;
    }
}
