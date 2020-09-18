package io.choerodon.agile.api.vo.report;

import java.util.List;

import io.choerodon.agile.api.vo.SearchVO;
import io.choerodon.agile.infra.utils.EncryptionUtils;

/**
 * @author jiaxu.cui@hand-china.com 2020/9/18 下午2:32
 */
public class DynamicListUnitVO extends ReportUnitVO {

    @Override
    public void validateAndconvert() {
        super.validateAndconvert();
        EncryptionUtils.decryptSearchVO(searchVO);
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
