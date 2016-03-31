Elm.Native.Table = {};
Elm.Native.Table.make = function(localRuntime) {
    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.Table = localRuntime.Native.Table || {};
    if (localRuntime.Native.Table.values)
    {
        return localRuntime.Native.Table.values;
    }

    var Maybe = Elm.Maybe.make(localRuntime);

    function fromList(list)
    {
        var temp = list;
        var len = 0;
        while (temp.ctor !== '[]')
        {
            temp = temp._1;
            ++len;
        }

        if (len < 1)
        {
            return [];
        }

        var table = new Array(len);
        var i = 0;

        while (list.ctor !== '[]')
        {
            table[i] = list._0;
            list = list._1;
            ++i;
        }

        return table;
    }

    function takeFromList(startIndex, list)
    {
        var temp = list;
        var len = 0;

        while (temp.ctor !== '[]' && len < startIndex)
        {
            temp = temp._1;
            ++len;
        }

        if (len < 1)
        {
            return {
                ctor: "_Tuple2",
                _0: [],
                _1: list
            };
        }

        var table = new Array(len);

        for (var i = 0; i < len; ++i)
        {
            table[i] = list._0;
            list = list._1;
        }

        return {
            ctor: "_Tuple2",
            _0: table,
            _1: list
        };
    }

    function toList(table)
    {
        var result = { ctor: '[]' };

        for (var i = table.length - 1; i >= 0; --i)
        {
            result = {
                ctor: '::',
                _0: table[i],
                _1: result
            };
        }

        return result;
    }

    var empty = [];

    function initialize(f, len)
    {
        if (len <= 0) {
            return [];
        }
        
        var result = new Array(len);

        for (var i = 0; i < len; ++i)
        {
            result[i] = f(i);
        }

        return result;
    }

    // This is complicated enough to warrant being rewritten in Elm
    // But how?
    function redistributeMany(size, tableOfTables)
    {
        var totalItems = 0;

        for (var i = 0; i < tableOfTables.length; ++i)
        {
            totalItems += tableOfTables[i].length;
        }

        var newTableCount = Math.ceil(totalItems / size);
        var resultTableOfTables = new Array(newTableCount);
        var tablesAdded = 0;
        var itemsLeft = totalItems;
        var lastTable;
        var lastTableItems = 0;

        function resetLastTable()
        {
            lastTableItems = 0;
            if (itemsLeft >= size)
            {
                lastTable = new Array(size);
            }
            else
            {
                lastTable = new Array(itemsLeft);
            }
        }

        resetLastTable();

        for (var i = 0; i < tableOfTables.length; ++i)
        {
            var table = tableOfTables[i];
            for (var j = 0; j < table.length; ++j)
            {
                if (lastTableItems === size)
                {
                    resultTableOfTables[tablesAdded++] = lastTable;
                    resetLastTable();
                }
                lastTable[lastTableItems++] = table[j];
                --itemsLeft;
            }
        }

        if (lastTableItems !== 0)
        {
            resultTableOfTables[tablesAdded++] = lastTable;
        }

        return resultTableOfTables;
    }

    // TODO: This can definitely be rewritten in terms of initialize
    function append(table1, table2)
    {
        return table1.concat(table2);
    }

    function get(i, table)
    {
        if (i < 0 || i >= table.length)
        {
            return Maybe.Nothing;
        }

        return Maybe.Just(table[i]);
    }

    function set(i, val, table)
    {
        if (i < 0 || i >= table.length)
        {
            return table;
        }

        var result = table.slice();
        result[i] = val;
        return result;
    }

    function push(val, table)
    {
        // it's a microptimization, but this should probably be profiled against
        // other ways of doing the same thing, like `slice()`ing and then `push()`ing,
        // or just iterating through a simple loop.
        return table.concat([val]);
    }

    function pushMany(list, table)
    {
        var temp = list;
        var len = 0;
        while (temp.ctor !== '[]')
        {
            temp = temp._1;
            ++len;
        }

        if (len < 1)
        {
            return table;
        }

        var result = new Array(table.length + len);
        var i = 0;

        for (; i < table.length; ++i)
        {
            result[i] = table[i];
        }

        while (list.ctor !== '[]')
        {
            result[i] = list._0;
            list = list._1;
            ++i;
        }

        return result;
    }

    function pushManyFrom(startIndex, list, table)
    {
        var temp = list;
        var len = 0;
        while (temp.ctor !== '[]')
        {
            temp = temp._1;
            ++len;
        }

        if (len < 1)
        {
            return table;
        }

        var keptItems = Math.min(table.length, startIndex);
        var result = new Array(keptItems + len);
        var i = 0;

        for (; i < keptItems; ++i)
        {
            result[i] = table[i];
        }

        while (list.ctor !== '[]')
        {
            result[i] = list._0;
            list = list._1;
            ++i;
        }

        return result;
    }

    function length(table)
    {
        return table.length;
    }

    function map(f, table)
    {
        // Is Array.prototype.map completely portable and supported in ancient browsers?
        // I'm going to play it safe and assume no.
        // Can someone more knowledgeable about the JS ecosystem address this?
        var result = new Array(table.length);

        for (var i = 0; i < table.length; ++i)
        {
            result[i] = f(table[i]);
        }

        return result;
    }

    function foldl(f, init, table)
    {
        // Ditto with map; should use reduce instead?
        var result = init;

        for (var i = 0; i < table.length; ++i)
        {
            result = A2(f, table[i], result);
        }

        return result;
    }

    function foldr(f, init, table)
    {
        // Ditto with map and foldl; should use reduceRight instead?
        var result = init;

        for (var i = table.length - 1; i >= 0; --i)
        {
            result = A2(f, table[i], result);
        }

        return result;
    }

    function mapAccumL(f, init, table)
    {
        var accumulator = init;
        var newTable = new Array(table.length);

        for (var i = 0; i < table.length; ++i)
        {
            var processedItem = A2(f, table[i], accumulator);
            accumulator = processedItem._0;
            newTable[i] = processedItem._1;
        }

        return {
            ctor: "_Tuple2",
            _0: accumulator,
            _1: newTable
        };
    }

    function findIndex(f, table)
    {
        for (var i = 0; i < table.length; ++i)
        {
            if (f(table[i]))
            {
                return Maybe.Just(i);
            }
        }

        return Maybe.Nothing;
    }

    return localRuntime.Native.Table.values = {
        fromList: fromList,
        takeFromList: F2(takeFromList),
        toList: toList,
        empty: empty,
        initialize: F2(initialize),
        redistributeMany: F2(redistributeMany),
        append: F2(append),

        get: F2(get),
        set: F3(set),

        push: F2(push),
        pushMany: F2(pushMany),
        pushManyFrom: F3(pushManyFrom),

        length: length,

        map: F2(map),
        foldl: F3(foldl),
        foldr: F3(foldr),
        mapAccumL: F3(mapAccumL),
        findIndex: F2(findIndex)
    };
}
