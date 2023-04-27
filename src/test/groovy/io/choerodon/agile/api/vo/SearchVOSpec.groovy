package io.choerodon.agile.api.vo


import spock.lang.Specification
import spock.lang.Unroll

class SearchVOSpec extends Specification {

    @Unroll
    def "testProcessContent 入参{issuePrefix = #issuePrefix, content = #content} 出参 {issueNumber = #issueNumberResult, summary = #summaryResult}"() {
        given: "构造测试数据"
        def searchVO = new SearchVO()
        searchVO.setContent(content)

        when: "执行测试"
        searchVO.processContent(issuePrefix)

        then: "验证返回结果"
        def pair = searchVO.getIssueNumberAndSummaryPair()
        pair?.first == issueNumberResult
        pair?.second == summaryResult

        where: "测试数据"
        content     | issuePrefix   || issueNumberResult    | summaryResult
        'yq-pm-123' | 'yq-pm'       || '123'                | 'yq-pm-123'
        'pm-123'    | 'yq-pm'       || '123'                | 'pm-123'
        '-123'      | 'yq-pm'       || '123'                | '-123'
        '-'         | 'yq-pm'       || ''                   | '-'
        'hello'     | 'yq-pm'       || 'hello'              | 'hello'
        'yq-pm-123-'| 'yq-pm'       || 'yq-pm-123-'         | 'yq-pm-123-'
        '666-123'   | 'yq-pm'       || '666-123'            | '666-123'
        null        | 'yq-pm'       || null                 | null
        'yq-pm-123' | null          || null                 | null
        null        | null          || null                 | null
    }

}
